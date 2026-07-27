(ns events-test
  (:require [clojure.set :as set]
            [cljs.test :refer-macros [deftest is testing]]
            [datascript.core :as ds :refer [transact! entity]]
            [ogres.app.cards :as cards]
            [ogres.app.const :refer [grid-size half-size hex-radius]]
            [ogres.app.crazy-eights :as crazy-eights]
            [ogres.app.dice :as dice]
            [ogres.app.events :refer [event-tx-fn]]
            [ogres.app.game-type :as game-type]
            [ogres.app.geom :as geom]
            [ogres.app.old-maid :as old-maid]
            [ogres.app.props :as props]
            [ogres.app.provider.state :refer [initial-data]]
            [ogres.app.vec :as vec :refer [Vec2]]))

(defn dispatch [conn event & args]
  (transact! conn [[:db.fn/call (fn [db] (apply event-tx-fn db event args))]]))

(defn user [conn]
  (entity @conn [:db/ident :user]))

(defn ^:private current-scene [conn]
  (:camera/scene (:user/camera (user conn))))

(defn ^:private set-enabled-elements!
  "Test helper: replaces the active scene's game-type's own
   :game-type/enabled-elements wholesale (a schema-free, single-value
   attribute -- map-form transact! replaces it, doesn't merge) so a
   test can exercise a specific combination of rule toggles, or (as
   every ported mini-game's own /start now requires) simply enable
   its own :X/game element, without switching to a seeded template at
   all."
  [conn elements]
  (let [game-type-id (:db/id (:scene/game-type (current-scene conn)))]
    (transact! conn [{:db/id game-type-id :game-type/enabled-elements (set elements)}])))

;; --- Mini-game sessions ---
(defn ^:private clear-cards-to-draw!
  "Test helper: relocates EVERY card in `deck` back to the draw pile,
   holder cleared -- run right after a mini-game session's own /start
   so tests can deal out an exact, deterministic hand instead of
   risking contamination from whatever the random initial deal
   produced. `deck` is a mini-game session's own :minigame/deck --
   deliberately NOT part of the scene's :scene/decks (see minigame-
   create-tx), unlike the generic Decks-panel tests' own current-deck."
  [conn deck]
  (let [cards (:deck/cards deck)]
    (transact! conn (mapcat (fn [c i] [{:db/id (:db/id c) :card/location :draw :card/position i}
                                        [:db/retract (:db/id c) :card/holder]])
                             cards (range)))))

(defn ^:private force-deck-top-of-draw!
  "Test helper: relocates one :draw-pile card of `rank` in `deck` to the
   highest :card/position (i.e. 'top of the pile') so the next draw is
   deterministic -- returns that card's :db/id. The session-scoped
   equivalent of force-top-of-draw! below."
  [conn deck rank]
  (let [draw (filter (comp #{:draw} :card/location) (:deck/cards deck))
        card (first (filter (comp #{rank} :card/rank) draw))
        max-pos (apply max (map :card/position draw))]
    (transact! conn [{:db/id (:db/id card) :card/position (inc max-pos)}])
    (:db/id card)))

(defn ^:private minigame-viewing-id
  "The :db/id of whichever session the dispatching connection just
   started -- every ported game's own /start sets the creator's own
   :user/minigame-viewing to the new session (see events.cljs's
   minigame-attach-tx), so this is a reliable way to grab THAT
   session's id even when more than one exists on the scene."
  [conn]
  (:db/id (:user/minigame-viewing (user conn))))

(deftest test-panel
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :user/select-panel :session)
    (is (= (:panel/selected (user conn)) :session))

    (dispatch conn :user/toggle-panel)
    (is (= (:panel/expanded (user conn)) false))

    (dispatch conn :user/toggle-panel)
    (is (= (:panel/expanded (user conn)) true))

    (dispatch conn :user/toggle-panel)
    (dispatch conn :user/select-panel :tokens)
    (is (= (:panel/expanded (user conn)) true))
    (is (= (:panel/selected (user conn)) :tokens))))

(deftest test-scene-focus
  (let [db (initial-data true)
        sc (:db/id (:camera/scene (:user/camera (entity db [:db/ident :user]))))
        tx [{:db/ident :root
             :root/session
             {:db/ident :session
              :session/host {:db/ident :user}
              :session/conns
              [{:user/host false :user/uuid (random-uuid) :user/cameras {:db/id -1 :camera/scene sc} :user/camera -1}
               {:user/host false :user/uuid (random-uuid) :user/cameras {:db/id -2 :camera/scene sc} :user/camera -2}
               {:user/host false :user/uuid (random-uuid) :user/cameras {:db/id -3 :camera/scene sc} :user/camera -3}]}}]
        conn (ds/conn-from-db (ds/db-with db tx))]
    (dispatch conn :scenes/create)
    (dispatch conn :session/focus)
    (let [{conns :session/conns} (entity @conn [:db/ident :session])]
      (is (every? (comp #{2} count :user/cameras) conns)
          "New cameras are created for users that don't already have one for the newly focused scene.")
      (is (every? #{(:camera/scene (:user/camera (user conn)))} (map (comp :camera/scene :user/camera) conns))
          "Every user is viewing the same scene as the host.")
      (is (= (count (into #{} (map :user/camera) conns)) 3)
          "Every user has a distinct camera entity."))
    (dispatch conn :scenes/change sc)
    (let [{conns :session/conns} (entity @conn [:db/ident :session])]
      (is (every? (comp not #{sc} :db/id :camera/scene :user/camera) conns)
          "Users remain on the scene even if the host changes theirs."))))

(deftest test-hex-pointy-grid-token-create
  (let [conn (ds/conn-from-db (initial-data true))
        point (Vec2. 3 -2)]
    (dispatch conn :scene/toggle-grid-align true)
    (dispatch conn :scene/change-grid-type :hex-pointy)
    (dispatch conn :token/create point nil)
    (let [scene (:camera/scene (:user/camera (user conn)))
          token (first (:scene/tokens scene))]
      (is (= (:scene/grid-type scene) :hex-pointy)
          "The scene's grid type was updated.")
      (is (= (:object/point token) (vec/nearest-hex point hex-radius))
          "A token created while aligned to a pointy-top hex grid snaps to
           the nearest hex center rather than the nearest square cell."))))

(deftest test-hex-flat-grid-token-create
  (let [conn (ds/conn-from-db (initial-data true))
        point (Vec2. 3 -2)]
    (dispatch conn :scene/toggle-grid-align true)
    (dispatch conn :scene/change-grid-type :hex-flat)
    (dispatch conn :token/create point nil)
    (let [scene (:camera/scene (:user/camera (user conn)))
          token (first (:scene/tokens scene))]
      (is (= (:scene/grid-type scene) :hex-flat)
          "The scene's grid type was updated.")
      (is (= (:object/point token) (vec/nearest-hex-flat point hex-radius))
          "A token created while aligned to a flat-top hex grid snaps to
           the nearest hex center rather than the nearest square cell."))))

(deftest test-iso-square-grid-token-create
  (let [conn (ds/conn-from-db (initial-data true))
        point (Vec2. 3 -2)]
    (dispatch conn :scene/toggle-grid-align true)
    (dispatch conn :scene/change-grid-type :iso-square)
    (dispatch conn :token/create point nil)
    (let [scene (:camera/scene (:user/camera (user conn)))
          token (first (:scene/tokens scene))
          logical (geom/screen->scene-vec point 1 :iso-square)
          expected (-> (vec/shift logical (- half-size))
                       (vec/rnd grid-size)
                       (vec/shift half-size))]
      (is (= (:scene/grid-type scene) :iso-square)
          "The scene's grid type was updated.")
      (is (= (:object/point token) expected)
          "A token created on an isometric square grid converts the click
           through the isometric inverse before snapping to the nearest
           square cell."))))

(deftest test-iso-hex-pointy-grid-token-create
  (let [conn (ds/conn-from-db (initial-data true))
        point (Vec2. 3 -2)]
    (dispatch conn :scene/toggle-grid-align true)
    (dispatch conn :scene/change-grid-type :iso-hex-pointy)
    (dispatch conn :token/create point nil)
    (let [scene (:camera/scene (:user/camera (user conn)))
          token (first (:scene/tokens scene))
          logical (geom/screen->scene-vec point 1 :iso-hex-pointy)]
      (is (= (:scene/grid-type scene) :iso-hex-pointy)
          "The scene's grid type was updated.")
      (is (= (:object/point token) (vec/nearest-hex logical hex-radius))
          "A token created on an isometric pointy-top hex grid converts the
           click through the isometric inverse before snapping to the
           nearest hex center."))))

(defn root [conn]
  (entity @conn [:db/ident :root]))

(deftest test-user-change-mode
  (let [conn (ds/conn-from-db (initial-data true))]
    (is (= (:user/mode (user conn)) nil)
        "The interface mode is unset until changed (defaults to :setup via
         pull default elsewhere).")
    (dispatch conn :user/change-mode :builder)
    (is (= (:user/mode (user conn)) :builder))
    (dispatch conn :user/change-mode :play)
    (is (= (:user/mode (user conn)) :play))))

(deftest test-game-type-seeded-default
  (let [conn (ds/conn-from-db (initial-data true))
        game-types (:root/game-types (root conn))
        default (first (filter (comp #{:default} :game-type/key) game-types))
        dnd5e (first (filter (comp #{:dnd5e} :game-type/key) game-types))
        gloomhaven (first (filter (comp #{:gloomhaven} :game-type/key) game-types))
        memory (first (filter (comp #{:memory} :game-type/key) game-types))
        go-fish (first (filter (comp #{:go-fish} :game-type/key) game-types))
        old-maid (first (filter (comp #{:old-maid} :game-type/key) game-types))
        crazy-eights (first (filter (comp #{:crazy-eights} :game-type/key) game-types))
        rummy (first (filter (comp #{:rummy} :game-type/key) game-types))
        war (first (filter (comp #{:war} :game-type/key) game-types))
        scene (:camera/scene (:user/camera (user conn)))]
    (is (= (count game-types) 9)
        "The bundled 'Default', 'D&D 5e', 'Gloomhaven', 'Memory', 'Go
         Fish', 'Old Maid', 'Crazy 8s', 'Rummy', and 'War' game-types
         are all seeded on a fresh db.")
    (is (= (:game-type/name default) "Default"))
    (is (= (:game-type/name dnd5e) "D&D 5e"))
    (is (= (:game-type/name gloomhaven) "Gloomhaven"))
    (is (= (:game-type/name memory) "Memory"))
    (is (= (:game-type/name go-fish) "Go Fish"))
    (is (= (:game-type/name old-maid) "Old Maid"))
    (is (= (:game-type/name crazy-eights) "Crazy 8s"))
    (is (= (:game-type/name rummy) "Rummy"))
    (is (= (:game-type/name war) "War"))
    (is (= (:game-type/category memory) "card")
        "Memory carries a template-picker grouping category -- the
         other three non-card-game seeded templates deliberately don't.")
    (is (= (:game-type/category go-fish) "card")
        "Go Fish shares Memory's 'card' category, grouping them together
         in the template picker.")
    (is (= (:game-type/category old-maid) "card")
        "Old Maid shares the same 'card' category too.")
    (is (= (:game-type/category crazy-eights) "card")
        "Crazy 8s shares the same 'card' category too.")
    (is (= (:game-type/category rummy) "card")
        "Rummy shares the same 'card' category too.")
    (is (= (:game-type/category war) "card")
        "War shares the same 'card' category too.")
    (is (some #(= (namespace %) "go-fish") (:game-type/enabled-elements go-fish))
        "The seeded Go Fish template enables its own module's elements.")
    (is (not (contains? (:game-type/enabled-elements go-fish) :unit/initiative))
        "Go Fish has its own turn-order UI, same reasoning as Memory's
         template -- the generic Turn Order tab is excluded.")
    (is (some #(= (namespace %) "old-maid") (:game-type/enabled-elements old-maid))
        "The seeded Old Maid template enables its own module's element.")
    (is (not (contains? (:game-type/enabled-elements old-maid) :unit/initiative))
        "Old Maid has its own turn-order UI too -- the generic Turn
         Order tab is excluded.")
    (is (some #(= (namespace %) "crazy-eights") (:game-type/enabled-elements crazy-eights))
        "The seeded Crazy 8s template enables its own module's element.")
    (is (not (contains? (:game-type/enabled-elements crazy-eights) :unit/initiative))
        "Crazy 8s has its own turn-order UI too -- the generic Turn
         Order tab is excluded.")
    (is (some #(= (namespace %) "rummy") (:game-type/enabled-elements rummy))
        "The seeded Rummy template enables its own module's element.")
    (is (not (contains? (:game-type/enabled-elements rummy) :rummy/runs))
        "runs stay off by default -- a real, independently-toggleable
         rule variant, not baked into the seeded template.")
    (is (not (contains? (:game-type/enabled-elements rummy) :unit/initiative))
        "Rummy has its own turn-order UI too -- the generic Turn Order
         tab is excluded.")
    (is (some #(= (namespace %) "war") (:game-type/enabled-elements war))
        "The seeded War template enables its own module's element.")
    (is (not (contains? (:game-type/enabled-elements war) :unit/initiative))
        "War has no per-token turn order at all -- everyone plays
         simultaneously -- but the generic Turn Order tab is still
         excluded, same as every other card game's template.")
    (is (contains? (:game-type/enabled-elements default) :unit/dead)
        "The seeded default enables the bare universal element set.")
    (is (not-any? #(contains? (:game-type/enabled-elements default) %)
                  [:tool/measurement :tool/measurement-cells :tool/mask :tool/shapes
                   :unit/size :unit/light :unit/aura])
        "The seeded default does not enable any of the opt-in shared
         primitives -- those are per-template opt-ins now, not universal.")
    (is (not-any? #(= (namespace %) "dnd5e")
                  (:game-type/enabled-elements default))
        "The seeded default has nothing game-specific enabled.")
    (is (some #(= (namespace %) "dnd5e") (:game-type/enabled-elements dnd5e))
        "The seeded D&D 5e template enables its own module's elements.")
    (is (some #(= (namespace %) "gloomhaven") (:game-type/enabled-elements gloomhaven))
        "The seeded Gloomhaven template enables its own module's elements.")
    (is (not (contains? (:game-type/enabled-elements gloomhaven) :unit/light))
        "Gloomhaven has no per-token light-radius mechanic -- unlike D&D's
         darkvision-driven one -- so the seeded template doesn't enable
         :unit/light, even though it does enable :unit/size.")
    (is (and (contains? (:game-type/enabled-elements gloomhaven) :tool/measurement-cells)
             (not (contains? (:game-type/enabled-elements gloomhaven) :tool/measurement)))
        "Gloomhaven's board doesn't use feet -- the seeded template enables the
         grid-cell measurement primitive instead of the real-world-unit one.")
    (is (and (contains? (:game-type/enabled-elements dnd5e) :tool/measurement)
             (not (contains? (:game-type/enabled-elements dnd5e) :tool/measurement-cells)))
        "D&D 5e measures in feet, the other way around -- the two measurement
         primitives aren't exclusive (a custom game-type could enable both), but
         neither seeded template turns on the one it doesn't use.")
    (is (= (:db/id (:scene/game-type scene)) (:db/id default))
        "A freshly created scene already references the seeded default.")))

(deftest test-user-edit-game-type-activates-scene
  (let [conn (ds/conn-from-db (initial-data true))
        default-id (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn)))))]
    (dispatch conn :game-type/create default-id "Custom")
    (let [custom-id (:db/id (:user/game-type-editing (user conn)))]
      (is (some? custom-id))
      (is (= (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn))))) custom-id)
          "Game types are isolated to Game Builder mode -- creating (and
           thus opening for editing) a game-type immediately makes it the
           active game-type for the current scene, with no separate
           scene-level picker.")
      (dispatch conn :user/edit-game-type default-id)
      (is (= (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn))))) default-id)
          "Selecting a different template in Game Builder mode switches
           the scene back to it."))))

(deftest test-game-type-create-clones-source
  (let [conn (ds/conn-from-db (initial-data true))
        default-id (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn)))))
        default-elements (:game-type/enabled-elements (entity @conn default-id))]
    (dispatch conn :game-type/create default-id "Custom")
    (let [custom-id (:db/id (:user/game-type-editing (user conn)))
          custom (entity @conn custom-id)]
      (is (= (set (:game-type/enabled-elements custom)) (set default-elements))
          "A newly created game-type clones its source's enabled elements.")
      (is (= (count (:root/game-types (root conn))) 10)
          "The new game-type is linked into :root/game-types alongside the
           nine bundled templates (Default, D&D 5e, Gloomhaven, Memory,
           Go Fish, Old Maid, Crazy 8s, Rummy, War)."))))

(deftest test-game-type-toggle-element
  (let [conn (ds/conn-from-db (initial-data true))
        game-type-id (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn)))))]
    (dispatch conn :game-type/toggle-element game-type-id :unit/light false)
    (is (not (contains? (:game-type/enabled-elements (entity @conn game-type-id)) :unit/light))
        "Toggling an element off removes it from the enabled set.")
    (dispatch conn :game-type/toggle-element game-type-id :unit/light true)
    (is (contains? (:game-type/enabled-elements (entity @conn game-type-id)) :unit/light)
        "Toggling an element back on restores it.")))

(deftest test-game-type-toggle-element-exclusive-group
  (let [conn (ds/conn-from-db (initial-data true))
        game-type-id (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn)))))]
    (dispatch conn :game-type/toggle-element game-type-id :dnd5e/hp-tracker true)
    (is (contains? (:game-type/enabled-elements (entity @conn game-type-id)) :dnd5e/hp-tracker))

    (dispatch conn :game-type/toggle-element game-type-id :gloomhaven/hp-tracker true)
    (let [enabled (:game-type/enabled-elements (entity @conn game-type-id))]
      (is (contains? enabled :gloomhaven/hp-tracker)
          "Enabling Gloomhaven's HP tracker succeeds.")
      (is (not (contains? enabled :dnd5e/hp-tracker))
          "...and automatically disables D&D 5e's, since they share an
           :exclusive-group and a game-type should never have two
           competing HP trackers enabled at once."))

    (dispatch conn :game-type/toggle-element game-type-id :gloomhaven/hp-tracker false)
    (is (not (contains? (:game-type/enabled-elements (entity @conn game-type-id)) :gloomhaven/hp-tracker))
        "Disabling an exclusive-group member never re-enables anything else --
         only enabling has the auto-disable side effect.")
    (is (not (contains? (:game-type/enabled-elements (entity @conn game-type-id)) :dnd5e/hp-tracker))
        "D&D 5e's tracker, already off from the earlier conflict, stays off.")))

(deftest test-game-type-toggle-category
  (let [conn (ds/conn-from-db (initial-data true))
        game-type-id (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn)))))
        dnd5e-ids (into #{} (filter #(= (namespace %) "dnd5e")) (keys game-type/elements))]
    (dispatch conn :game-type/toggle-category game-type-id dnd5e-ids true)
    (is (every? (:game-type/enabled-elements (entity @conn game-type-id)) dnd5e-ids)
        "Enabling a whole category turns on every one of its elements in
         a single call -- the 'select all D&D 5e features' toggle.")

    (dispatch conn :game-type/toggle-category game-type-id dnd5e-ids false)
    (is (not-any? (:game-type/enabled-elements (entity @conn game-type-id)) dnd5e-ids)
        "Disabling a whole category turns off every one of its elements.")))

(deftest test-game-type-toggle-category-exclusive-group
  (let [conn (ds/conn-from-db (initial-data true))
        game-type-id (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn)))))
        dnd5e-ids (into #{} (filter #(= (namespace %) "dnd5e")) (keys game-type/elements))]
    (dispatch conn :game-type/toggle-element game-type-id :gloomhaven/hp-tracker true)
    (dispatch conn :game-type/toggle-category game-type-id dnd5e-ids true)
    (let [enabled (:game-type/enabled-elements (entity @conn game-type-id))]
      (is (contains? enabled :dnd5e/hp-tracker)
          "Enabling the whole D&D 5e category includes its HP tracker.")
      (is (not (contains? enabled :gloomhaven/hp-tracker))
          "...which evicts Gloomhaven's HP tracker -- the same
           :exclusive-group conflict resolution :game-type/toggle-element
           already applies, just per-id within the batch."))))

(deftest test-game-type-toggle-category-grid-switches-scene
  (let [conn (ds/conn-from-db (initial-data true))
        game-type-id (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn)))))]
    (is (= (:scene/grid-type (:camera/scene (:user/camera (user conn))) :square) :square)
        "The scene starts on the default (unset -> :square) grid-type.")
    (dispatch conn :game-type/toggle-category game-type-id #{:tool/grid-square} false)
    (is (not= (:scene/grid-type (:camera/scene (:user/camera (user conn)))) :square)
        "Disabling a category containing the scene's current grid-type
         switches it to another available option, same as the
         single-element event's grid-switch tail.")))

(deftest test-game-type-toggle-category-with-disable-ids
  (let [conn (ds/conn-from-db (initial-data true))
        game-type-id (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn)))))
        gloomhaven-ids (into #{} (filter #(= (namespace %) "gloomhaven")) (keys game-type/elements))
        other-grids (disj (set game-type/grid-order) :tool/grid-hex-pointy)]
    ;; Default seeds every grid layout enabled -- simulates the Builder's
    ;; "Gloomhaven" category checkbox, which folds :tool/grid-hex-pointy
    ;; into its own scope and queues every other grid as disable-ids.
    (dispatch conn :game-type/toggle-category game-type-id
              (conj gloomhaven-ids :tool/grid-hex-pointy) true other-grids)
    (let [enabled (:game-type/enabled-elements (entity @conn game-type-id))]
      (is (every? enabled gloomhaven-ids)
          "Enabling the category still turns on every one of its own elements.")
      (is (contains? enabled :tool/grid-hex-pointy)
          "...and the module's preferred grid layout.")
      (is (not-any? enabled other-grids)
          "...while force-disabling every other grid layout in the same
           transaction, instead of just adding hex-pointy alongside
           whatever grids happened to already be enabled.")
      (is (= (:scene/grid-type (:camera/scene (:user/camera (user conn)))) :hex-pointy)
          "With every other grid layout disabled, the scene (previously
           on :square, which is no longer enabled) switches to the one
           remaining available option -- the same grid-switch tail
           :game-type/toggle-category already has."))

    ;; Disabling the category leaves the grid layouts alone -- there's
    ;; nothing to exclude when turning things off. Only gloomhaven-ids
    ;; itself is toggled here (not :tool/grid-hex-pointy, unlike the
    ;; enable call above) -- that's the whole point of this assertion.
    (dispatch conn :game-type/toggle-category game-type-id gloomhaven-ids false)
    (is (contains? (:game-type/enabled-elements (entity @conn game-type-id)) :tool/grid-hex-pointy)
        "Disabling never force-disables anything beyond the given ids --
         :tool/grid-hex-pointy (passed in the disable set on enable, not
         here) stays exactly as it was left.")))

(deftest test-game-type-toggle-grid-element-switches-scene
  (let [conn (ds/conn-from-db (initial-data true))
        game-type-id (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn)))))]
    (is (= (:scene/grid-type (:camera/scene (:user/camera (user conn))) :square) :square)
        "The scene starts on the default (unset -> :square) grid-type.")
    (dispatch conn :game-type/toggle-element game-type-id :tool/grid-square false)
    (let [scene (:camera/scene (:user/camera (user conn)))]
      (is (not= (:scene/grid-type scene) :square)
          "Disabling the scene's current grid-type element automatically
           switches it to another available option.")
      (is (contains? (:game-type/enabled-elements (entity @conn game-type-id))
                      (game-type/grid-tool-id (:scene/grid-type scene)))
          "The scene lands on a grid-type whose element is still enabled."))))

(def ^:private all-grid-elements
  (filter game-type/grid-type-from-tool-id (keys game-type/elements)))

(deftest test-game-type-toggle-all-grid-elements-off-leaves-scene-untouched
  (let [conn (ds/conn-from-db (initial-data true))
        game-type-id (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn)))))]
    (doseq [element-id all-grid-elements]
      (dispatch conn :game-type/toggle-element game-type-id element-id false))
    (is (= (game-type/grid-count (:game-type/enabled-elements (entity @conn game-type-id))) 0)
        "Every grid element is now disabled on the game-type (non-grid
         elements are untouched by this loop).")
    (let [scene (:camera/scene (:user/camera (user conn)))]
      (is (= (:scene/show-grid scene :default) :default)
          "No-grid mode does NOT force :scene/show-grid -- it's never
           destructively overwritten, so there's nothing to restore later
           (see test-game-type-re-enable-grid-element-restores-scene).
           Whether the grid is actually hidden while unavailable is a
           rendering concern (scene.cljs's `has-grid?`), not a stored
           attribute change.")
      (is (= (:scene/grid-align scene :default) :default)
          "Same for :scene/grid-align -- untouched."))))

(deftest test-game-type-re-enable-grid-element-restores-scene
  (let [conn (ds/conn-from-db (initial-data true))
        game-type-id (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn)))))]
    (doseq [element-id all-grid-elements]
      (dispatch conn :game-type/toggle-element game-type-id element-id false))
    (dispatch conn :game-type/toggle-element game-type-id :tool/grid-hex-flat true)
    (let [scene (:camera/scene (:user/camera (user conn)))]
      (is (= (:scene/grid-type scene) :hex-flat)
          "Re-enabling a grid option after every option was disabled
           switches the scene onto that newly available grid.")
      (is (= (:scene/show-grid scene :default) :default)
          "Still untouched -- since it was never forced to false, there's
           nothing to 'restore': whatever the user's own preference was
           (the default true, here) is simply correct again the instant
           a grid is available -- see test-no-grid-overrides-align-for-
           snapping and scene.cljs's `has-grid?` for how that's enforced
           without needing this event to know or care."))))

(deftest test-no-grid-mode-overrides-align-for-snapping
  (let [conn (ds/conn-from-db (initial-data true))
        game-type-id (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn)))))
        point (Vec2. 3 -2)]
    (dispatch conn :scene/toggle-grid-align true)
    (doseq [element-id all-grid-elements]
      (dispatch conn :game-type/toggle-element game-type-id element-id false))
    (dispatch conn :token/create point nil)
    (let [scene (:camera/scene (:user/camera (user conn)))
          token (first (:scene/tokens scene))
          ;; The disable loop above may leave :scene/grid-type on
          ;; whichever option was last available before the count hit
          ;; zero (including an iso variant, which applies a real
          ;; coordinate projection independent of alignment) -- so the
          ;; "no snapping happened" expectation has to go through the
          ;; same base screen->scene conversion the event itself applies,
          ;; not assume the raw input point is preserved verbatim.
          grid-type (:scene/grid-type scene :square)
          expected (geom/screen->scene-vec point 1 grid-type)]
      (is (= (:scene/grid-align scene) true)
          "The scene's own 'align to grid' preference is untouched and
           still true...")
      (is (= (:object/point token) expected)
          "...but with zero grid layouts enabled on the active game-type,
           only the base coordinate conversion is applied -- no
           additional grid-cell snapping happens -- 'no-grid mode'
           overrides align regardless of the scene's stored preference."))))

(deftest test-re-enabled-grid-resumes-snapping-immediately
  (let [conn (ds/conn-from-db (initial-data true))
        game-type-id (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn)))))
        point (Vec2. 3 -2)]
    (dispatch conn :scene/toggle-grid-align true)
    (doseq [element-id all-grid-elements]
      (dispatch conn :game-type/toggle-element game-type-id element-id false))
    ;; Re-enabling a grid restores snapping immediately, with no separate
    ;; "turn align back on" step, since it was never turned off.
    (dispatch conn :game-type/toggle-element game-type-id :tool/grid-square true)
    (dispatch conn :token/create point nil)
    (let [scene (:camera/scene (:user/camera (user conn)))
          token (first (:scene/tokens scene))]
      (is (not= (:object/point token) point)
          "Snapping resumes as soon as a grid becomes available again."))))

(deftest test-game-type-toggle-additional-grid-element-is-a-no-op-for-scene
  (let [conn (ds/conn-from-db (initial-data true))
        game-type-id (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn)))))]
    (dispatch conn :scene/change-grid-type :hex-flat)
    ;; Disabling an element the scene ISN'T currently using, while other
    ;; grid options remain, shouldn't touch the scene at all.
    (dispatch conn :game-type/toggle-element game-type-id :tool/grid-square false)
    (let [scene (:camera/scene (:user/camera (user conn)))]
      (is (= (:scene/grid-type scene) :hex-flat))
      (is (= (:scene/show-grid scene :default) :default)
          "Untouched -- :scene/show-grid is never explicitly set/forced by
           this unrelated toggle, so it stays absent (defaulting to true
           for rendering purposes elsewhere via pull defaults).")
      (is (= (:scene/grid-align scene :default) :default)
          "Untouched -- :scene/grid-align is never explicitly set/forced
           either way by this unrelated toggle."))))

(deftest test-game-type-set-icon-override
  (let [conn (ds/conn-from-db (initial-data true))
        game-type-id (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn)))))]
    (dispatch conn :game-type/set-icon-override game-type-id :unit/dead {:icon/url "https://example.com/icon.svg"})
    (is (= (get (:game-type/icon-overrides (entity @conn game-type-id)) :unit/dead)
           {:icon/url "https://example.com/icon.svg"})
        "Setting an icon override stores the link under the element id.")
    (dispatch conn :game-type/set-icon-override game-type-id :unit/dead nil)
    (is (not (contains? (:game-type/icon-overrides (entity @conn game-type-id)) :unit/dead))
        "Passing nil clears the override, falling back to the registry default.")))

(deftest test-game-type-rename
  (let [conn (ds/conn-from-db (initial-data true))
        game-type-id (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn)))))]
    (dispatch conn :game-type/rename game-type-id "  Renamed  ")
    (is (= (:game-type/name (entity @conn game-type-id)) "Renamed")
        "Renaming trims surrounding whitespace, matching :token/change-label.")))

(deftest test-game-type-remove-reassigns-scene-and-editing
  (let [conn (ds/conn-from-db (initial-data true))
        default-id (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn)))))]
    (dispatch conn :game-type/create default-id "Custom")
    (let [custom-id (:db/id (:user/game-type-editing (user conn)))]
      (is (= (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn))))) custom-id)
          "Sanity check: the scene and editing selection are on Custom.")
      (dispatch conn :game-type/remove custom-id)
      (is (nil? (:game-type/name (entity @conn custom-id)))
          "The removed game-type entity is gone.")
      (is (= (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn))))) default-id)
          "The scene falls back to the remaining (Default) game-type.")
      (is (= (:db/id (:user/game-type-editing (user conn))) default-id)
          "Builder mode's 'currently editing' selection falls back too."))))

(deftest test-game-type-remove-refuses-to-remove-the-last-one
  (let [conn (ds/conn-from-db (initial-data true))
        default-id (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn)))))
        others (remove #{default-id} (map :db/id (:root/game-types (root conn))))]
    ;; Three templates are seeded by default (Default, D&D 5e, Gloomhaven)
    ;; -- remove the other two first so this is actually testing the "last
    ;; one" invariant, not just the first of several removals succeeding.
    (doseq [id others] (dispatch conn :game-type/remove id))
    (is (= (count (:root/game-types (root conn))) 1)
        "Only the Default game-type remains after removing the others.")
    (dispatch conn :game-type/remove default-id)
    (is (= (count (:root/game-types (root conn))) 1)
        "Removing the only remaining game-type is a no-op.")
    (is (some? (:game-type/name (entity @conn default-id)))
        "The Default game-type is still there.")))

(deftest test-game-type-import
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :game-type/import
              {:name "Imported"
               :enabled-elements #{:unit/light :tool/grid-hex-flat :not/a-real-element}
               :icon-overrides {:unit/light {:icon/url "https://example.com/a.svg"}
                                :not/a-real-element {:icon/url "https://example.com/b.svg"}}})
    (let [imported-id (:db/id (:user/game-type-editing (user conn)))
          imported (entity @conn imported-id)]
      (is (= (:game-type/name imported) "Imported"))
      (is (= (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn))))) imported-id)
          "Importing opens the new template for editing and activates it
           on the current scene, same as creating one.")
      (is (= (:game-type/enabled-elements imported) #{:unit/light :tool/grid-hex-flat})
          "Unrecognized element ids are dropped by the caller's
           sanitization before this event ever sees them (simulated here
           by passing already-sanitized data, matching what the Builder
           panel's import handler does)."))))

(deftest test-initiative-change-rank
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :token/create (Vec2. 0 0) nil)
    (let [id (:db/id (first (:scene/tokens (:camera/scene (:user/camera (user conn))))))]
      (dispatch conn :initiative/change-rank id 14)
      (is (= (:initiative/rank (entity @conn id)) 14)
          "Sets an explicit turn-order rank -- the base, game-agnostic
           value every assignment mechanism (manual entry, :initiative/
           move, or a game module's own roll) ultimately writes.")
      (dispatch conn :initiative/change-rank id nil)
      (is (nil? (:initiative/rank (entity @conn id)))
          "nil clears the rank back to unranked."))))

(deftest test-initiative-assign-ranks
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :token/create (Vec2. 0 0) nil)
    (dispatch conn :token/create (Vec2. 10 10) nil)
    (let [[id1 id2] (map :db/id (:scene/tokens (:camera/scene (:user/camera (user conn)))))]
      (dispatch conn :initiative/assign-ranks {id1 5 id2 12})
      (is (= (:initiative/rank (entity @conn id1)) 5))
      (is (= (:initiative/rank (entity @conn id2)) 12)
          "Batch-writes every entry in one transaction -- this is what a
           bulk action (e.g. D&D's 'Roll Initiative for NPCs') dispatches
           instead of N separate :initiative/change-rank calls."))))

(deftest test-initiative-move
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :token/create (Vec2. 0 0) nil)
    (dispatch conn :token/create (Vec2. 10 10) nil)
    (dispatch conn :token/create (Vec2. 20 20) nil)
    (let [idxs (map :db/id (:scene/tokens (:camera/scene (:user/camera (user conn)))))]
      (dispatch conn :initiative/toggle idxs true))
    (let [scene (:camera/scene (:user/camera (user conn)))
          ;; Nobody has a rank yet -- initiative-order's nil fallback sorts
          ;; by :db/id descending, so this mirrors that starting order
          ;; directly.
          [top mid bottom] (map :db/id (sort-by :db/id > (:scene/initiative scene)))]
      (is (every? nil? (map :initiative/rank (:scene/initiative scene)))
          "Sanity check: this is the all-nil floor case.")

      ;; Move the bottom token to the very front, two slots up.
      (dispatch conn :initiative/move bottom :earlier)
      (dispatch conn :initiative/move bottom :earlier)
      (let [scene (:camera/scene (:user/camera (user conn)))
            reordered (map :db/id (sort-by (comp - :initiative/rank) (:scene/initiative scene)))]
        (is (= reordered [bottom top mid])
            "Two :earlier moves walk the token all the way to the front.")
        (is (= (set (map :initiative/rank (:scene/initiative scene))) #{1 2 3})
            "The whole list gets a clean contiguous descending rank by
             final position on every move, not just the two tokens that
             swapped -- the first move already ranks everyone at once."))

      ;; A third :earlier move is a no-op -- already at the front.
      (dispatch conn :initiative/move bottom :earlier)
      (is (= (:initiative/rank (entity @conn bottom)) 3)
          "Moving past the boundary doesn't change anything.")

      ;; Confirm the reorder actually drives turn advancement, not just
      ;; the panel's display order.
      (dispatch conn :initiative/next) ;; starts round 1
      (dispatch conn :initiative/next) ;; marks the first turn
      (is (= (:db/id (:initiative/turn (:camera/scene (:user/camera (user conn))))) bottom)
          "After manually moving it to the front, :initiative/next
           advances to that token first."))))

(deftest test-initiative-two-player-fixed-alternating-order
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :token/create (Vec2. 0 0) nil)
    (dispatch conn :token/create (Vec2. 10 10) nil)
    (let [idxs (map :db/id (:scene/tokens (:camera/scene (:user/camera (user conn)))))]
      (dispatch conn :initiative/toggle idxs true))
    (let [scene (:camera/scene (:user/camera (user conn)))
          [p1 p2] (map :db/id (sort-by :db/id > (:scene/initiative scene)))
          current-turn #(:db/id (:initiative/turn (:camera/scene (:user/camera (user conn)))))]
      ;; The floor case: two participants, nobody ever rolls or manually
      ;; assigns a rank -- e.g. a two-player game like chess with strictly
      ;; alternating fixed turns. This has to just work from the stable
      ;; :db/id tiebreak alone, with zero setup.
      (dispatch conn :initiative/next) ;; round 1 starts
      (dispatch conn :initiative/next) ;; p1's turn
      (is (= (current-turn) p1))
      (dispatch conn :initiative/next) ;; p2's turn
      (is (= (current-turn) p2))
      (dispatch conn :initiative/next) ;; everyone's played -- round 2 starts
      (is (= (:initiative/rounds (:camera/scene (:user/camera (user conn)))) 2))
      (dispatch conn :initiative/next) ;; p1's turn again
      (is (= (current-turn) p1)
          "With no ranks ever set, round 2 repeats the exact same order as
           round 1 -- p1, p2, p1, p2, forever -- requiring zero rolling or
           manual reordering. This is the floor the turn system has to
           support: the simplest possible game (two players, strictly
           alternating, e.g. chess) just works.")
      (dispatch conn :initiative/next) ;; p2's turn again
      (is (= (current-turn) p2)))))

(deftest test-camera-translate-non-iso
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :camera/translate (Vec2. 70 140))
    (is (= (:camera/point (:user/camera (user conn))) (Vec2. 70 140))
        "For a non-iso grid-type, the screen delta just divides by scale
         (1 here) unchanged, matching the pre-existing behavior.")))

(deftest test-camera-translate-applies-iso-inverse-projection
  (doseq [grid-type [:iso-square :iso-hex-pointy :iso-hex-flat
                      :iso-square-vertical :iso-hex-pointy-vertical :iso-hex-flat-vertical]]
    (let [conn (ds/conn-from-db (initial-data true))
          delta (Vec2. 70 140)]
      (dispatch conn :scene/change-grid-type grid-type)
      (dispatch conn :camera/translate delta)
      (let [expected (geom/screen->scene-vec delta 1 grid-type)]
        (is (= (:camera/point (:user/camera (user conn))) expected)
            (str "For " grid-type ", the committed camera position applies "
                 "the same isometric inverse projection the on-screen "
                 "render transform applies -- this is the fix for the "
                 "camera-pan 'jumps in the opposite direction on release' "
                 "bug (:camera/translate previously divided by scale only, "
                 "never applying the iso inverse at all; the error was "
                 "most visible on the 'vertical' variants because their "
                 "extra 90-degree rotation made the missing correction "
                 "look like an outright reversed drag direction)."))))))

;; --- Cards / Decks ---
(defn ^:private current-deck [conn]
  (first (:scene/decks (:camera/scene (:user/camera (user conn))))))

(defn ^:private by-location [deck location]
  (filter (comp #{location} :card/location) (:deck/cards deck)))

(deftest test-deck-create
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :deck/create :standard-52)
    (let [deck (current-deck conn)]
      (is (= (:deck/name deck) "Standard 52-Card Deck"))
      (is (= (count (:deck/cards deck)) 52)
          "extras (jokers) are excluded unless asked for")
      (is (every? (comp #{:draw} :card/location) (:deck/cards deck))
          "every card starts in the draw pile")
      (is (= (count (into #{} (map :card/position) (:deck/cards deck))) 52)
          "every card has a distinct position -- a real shuffle, not ties"))))

(deftest test-deck-create-with-extras
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :deck/create :standard-52 {:include-extras? true})
    (is (= (count (:deck/cards (current-deck conn))) 54)
        "the standard 52 plus 2 jokers")))

(deftest test-deck-draw
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :deck/create :standard-52)
    (let [deck-id (:db/id (current-deck conn))]
      (dispatch conn :deck/draw deck-id)
      (let [deck (entity @conn deck-id)]
        (is (= (count (by-location deck :draw)) 51))
        (is (= (count (by-location deck :discard)) 1))))))

(deftest test-deck-draw-auto-reshuffles-when-draw-pile-empties
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :deck/create :standard-52)
    (let [deck-id (:db/id (current-deck conn))]
      (dotimes [_ 52] (dispatch conn :deck/draw deck-id))
      (let [deck (entity @conn deck-id)]
        (is (= (count (by-location deck :discard)) 52)
            "every card has been discarded once")
        (is (= (count (by-location deck :draw)) 0)))
      (dispatch conn :deck/draw deck-id)
      (let [deck (entity @conn deck-id)]
        (is (= (count (by-location deck :draw)) 51)
            "the empty draw pile auto-reshuffled the discard pile back in,
             then the draw proceeded, all in one dispatch")
        (is (= (count (by-location deck :discard)) 1))))))

(deftest test-deck-draw-into-hand
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :deck/create :standard-52)
    (let [deck-id (:db/id (current-deck conn))
          host-id (:db/id (user conn))]
      (dispatch conn :deck/draw deck-id [:hand host-id])
      (let [deck (entity @conn deck-id)
            held (first (by-location deck :hand))]
        (is (some? held) "a card landed in the hand location")
        (is (= (:db/id (:card/holder held)) host-id))))))

(deftest test-deck-discard
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :deck/create :standard-52)
    (let [deck-id (:db/id (current-deck conn))
          host-id (:db/id (user conn))]
      (dispatch conn :deck/draw deck-id [:hand host-id])
      (let [held (first (by-location (entity @conn deck-id) :hand))]
        (dispatch conn :deck/discard (:db/id held))
        (let [card (entity @conn (:db/id held))]
          (is (= (:card/location card) :discard))
          (is (nil? (:card/holder card))
              "holder is cleared when a card leaves a hand"))))))

(deftest test-deck-deal
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :deck/create :standard-52)
    (let [deck-id (:db/id (current-deck conn))
          host-id (:db/id (user conn))]
      (dispatch conn :deck/deal deck-id [host-id] 5)
      (let [hand (by-location (entity @conn deck-id) :hand)]
        (is (= (count hand) 5))
        (is (every? (comp #{host-id} :db/id :card/holder) hand))))))

(deftest test-deck-reset
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :deck/create :standard-52)
    (let [deck-id (:db/id (current-deck conn))
          host-id (:db/id (user conn))]
      (dispatch conn :deck/deal deck-id [host-id] 5)
      (dispatch conn :deck/draw deck-id)
      (dispatch conn :deck/reset deck-id)
      (let [deck (entity @conn deck-id)]
        (is (every? (comp #{:draw} :card/location) (:deck/cards deck))
            "every card -- drawn, discarded, or held -- is back in the draw pile")
        (is (every? (comp nil? :card/holder) (:deck/cards deck)))
        (is (= (count (into #{} (map :card/position) (:deck/cards deck))) 52)
            "freshly shuffled, not just moved with stale positions")))))

(deftest test-deck-remove
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :deck/create :standard-52)
    (let [deck-id (:db/id (current-deck conn))
          card-id (:db/id (first (:deck/cards (entity @conn deck-id))))]
      (dispatch conn :deck/remove deck-id)
      (is (nil? (:db/id (entity @conn deck-id))) "the deck entity is gone")
      (is (nil? (:card/location (entity @conn card-id)))
          "its cards were retracted too, via :deck/cards' isComponent cleanup")
      (is (empty? (:scene/decks (:camera/scene (:user/camera (user conn)))))))))

;; --- Players ---
(defn ^:private root-players [conn]
  (:root/players (root conn)))

(deftest test-player-create
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :player/create :human)
    (let [player (first (root-players conn))]
      (is (= (:player/name player) "Player 1"))
      (is (= (:player/kind player) :human))
      (is (= (:player/color player) "red") "the first human palette color")
      (is (:player/active player) "created active by default"))))

(deftest test-player-create-npc-uses-muted-palette
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :player/create :npc)
    (let [player (first (root-players conn))]
      (is (= (:player/color player) "npc-red")
          "NPCs draw from their own disjoint, muted palette"))))

(deftest test-player-create-per-kind-numbering
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :npc)
    (let [names (into #{} (map :player/name) (root-players conn))]
      (is (= names #{"Player 1" "Player 2" "NPC 1"})
          "each kind numbers independently, not globally"))))

(deftest test-player-create-color-skips-taken-within-kind
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :npc)
    (let [players (root-players conn)
          humans (into [] (map :player/color) (filter (comp #{:human} :player/kind) players))
          npcs (into [] (map :player/color) (filter (comp #{:npc} :player/kind) players))]
      (is (= (set humans) #{"red" "blue"})
          "two humans never start with the same color")
      (is (= npcs ["npc-red"])
          "an NPC's color comes from its own pool, unaffected by human colors taken"))))

(deftest test-player-change-color-rejects-conflict-within-kind
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [[a b] (seq (root-players conn))]
      (dispatch conn :player/change-color (:db/id b) (:player/color a))
      (is (not= (:player/color (entity @conn (:db/id b))) (:player/color a))
          "changing to a color another same-kind player already has is a no-op")
      (dispatch conn :player/change-color (:db/id b) "teal")
      (is (= (:player/color (entity @conn (:db/id b))) "teal")
          "changing to a genuinely free color succeeds"))))

(deftest test-player-rename
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :player/create :human)
    (let [id (:db/id (first (root-players conn)))]
      (dispatch conn :player/rename id "Aramis")
      (is (= (:player/name (entity @conn id)) "Aramis")))))

(deftest test-player-change-kind-reassigns-color
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :player/create :human)
    (let [id (:db/id (first (root-players conn)))]
      (is (= (:player/color (entity @conn id)) "red"))
      (dispatch conn :player/change-kind id :npc)
      (is (= (:player/kind (entity @conn id)) :npc))
      (is (= (:player/color (entity @conn id)) "npc-red")
          "switching kind reassigns a color from the new kind's own palette"))))

(deftest test-player-change-kind-noop-when-unchanged
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :player/create :human)
    (let [id (:db/id (first (root-players conn)))]
      (dispatch conn :player/change-kind id :human)
      (is (= (:player/color (entity @conn id)) "red")
          "no-op when the kind doesn't actually change -- color untouched"))))

(deftest test-player-set-active
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :player/create :human)
    (let [id (:db/id (first (root-players conn)))]
      (dispatch conn :player/set-active id false)
      (is (false? (:player/active (entity @conn id))) "benched")
      (is (= (:player/color (entity @conn id)) "red") "color untouched while benched")
      (dispatch conn :player/set-active id true)
      (is (:player/active (entity @conn id)) "restored")
      (is (= (:player/name (entity @conn id)) "Player 1") "name untouched"))))

(deftest test-player-create-color-reserved-when-benched
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :player/create :human)
    (let [id (:db/id (first (root-players conn)))]
      (dispatch conn :player/set-active id false)
      (dispatch conn :player/create :human)
      (let [colors (into #{} (map :player/color) (root-players conn))]
        (is (= colors #{"red" "blue"})
            "a benched player's color still counts as taken for a new player")))))

(deftest test-player-remove
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :npc)
    (let [[a b] (seq (root-players conn))]
      (dispatch conn :player/remove (:db/id a))
      (is (nil? (:db/id (entity @conn (:db/id a)))) "the removed player entity is gone")
      (is (= (into #{} (map :db/id) (root-players conn)) #{(:db/id b)})
          "the other player is untouched")
      (dispatch conn :player/create :human)
      (is (= (:player/color (first (filter (comp #{:human} :player/kind) (root-players conn))))
             "red")
          "the removed player's color is available again for a new one"))))

(deftest test-player-roster-global-across-scenes
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :player/create :human)
    (dispatch conn :scenes/create)
    (is (= (count (root-players conn)) 1)
        "the roster is shared -- creating a new scene doesn't reset or duplicate it")))

;; --- Player/object visibility authority ---
(defn ^:private scene-token [conn]
  (first (:scene/tokens (:camera/scene (:user/camera (user conn))))))

(defn ^:private add-conn!
  "Test helper -- upserts a fake connected guest (:user/host false) with
   the given :user/uuid into :root/session's :session/conns, mirroring
   the pattern already used by test-scene-focus above."
  [conn uuid]
  (transact! conn [{:db/ident :root
                     :root/session
                     {:db/ident :session
                      :session/conns [{:user/host false :user/uuid uuid}]}}]))

(deftest test-player-set-controller
  (let [conn (ds/conn-from-db (initial-data true))
        guest-uuid (random-uuid)]
    (dispatch conn :player/create :npc)
    (let [player-id (:db/id (first (root-players conn)))]
      (add-conn! conn guest-uuid)
      (dispatch conn :player/set-controller player-id [:user/uuid guest-uuid])
      (is (= (:user/uuid (:player/controller (entity @conn player-id))) guest-uuid))
      (dispatch conn :player/set-controller player-id nil)
      (is (nil? (:player/controller (entity @conn player-id)))
          "clearing the controller reverts to host-controlled"))))

(deftest test-objects-assign-owner
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :token/create (Vec2. 0 0) nil)
    (dispatch conn :player/create :npc)
    (let [token-id (:db/id (scene-token conn))
          player-id (:db/id (first (root-players conn)))]
      (dispatch conn :objects/assign-owner [token-id] player-id)
      (is (= (:db/id (:object/owner (entity @conn token-id))) player-id))
      (dispatch conn :objects/assign-owner [token-id] nil)
      (is (nil? (:object/owner (entity @conn token-id)))
          "unassigning clears :object/owner"))))

(deftest test-objects-assign-alt-image
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :token-images/create-many [[{:hash "abc" :name "back" :size 1 :width 1 :height 1}
                                                {:hash "abc" :name "back" :size 1 :width 1 :height 1}]])
    (dispatch conn :token/create (Vec2. 0 0) nil)
    (let [token-id (:db/id (scene-token conn))]
      (dispatch conn :objects/assign-alt-image [token-id] :token/image-alt "abc")
      (is (= (:image/hash (:token/image-alt (entity @conn token-id))) "abc"))
      (dispatch conn :objects/assign-alt-image [token-id] :token/image-alt nil)
      (is (nil? (:token/image-alt (entity @conn token-id)))
          "clearing the placeholder image retracts it"))))

(deftest test-objects-toggle-hidden-host-allowed-when-unassigned
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :token/create (Vec2. 0 0) nil)
    (let [id (:db/id (scene-token conn))]
      (dispatch conn :objects/toggle-hidden id)
      (is (:object/hidden (entity @conn id))
          "the host may hide/reveal an unassigned object, same as before this feature"))))

(deftest test-objects-toggle-hidden-host-rejected-once-controller-assigned
  (let [conn (ds/conn-from-db (initial-data true))
        guest-uuid (random-uuid)]
    (dispatch conn :token/create (Vec2. 0 0) nil)
    (dispatch conn :player/create :npc)
    (let [id (:db/id (scene-token conn))
          player-id (:db/id (first (root-players conn)))]
      (add-conn! conn guest-uuid)
      (dispatch conn :objects/assign-owner [id] player-id)
      (dispatch conn :player/set-controller player-id [:user/uuid guest-uuid])
      (dispatch conn :objects/toggle-hidden id)
      (is (not (:object/hidden (entity @conn id)))
          "the host's own toggle attempt is rejected once a connected
           controller owns the object -- authority belongs to the
           controller alone, this is what lets a player hide something
           from the host"))))

(deftest test-objects-toggle-hidden-controller-allowed
  (let [conn (ds/conn-from-db (initial-data false))
        my-uuid (random-uuid)]
    (transact! conn [{:db/id [:db/ident :user] :user/uuid my-uuid}])
    (dispatch conn :token/create (Vec2. 0 0) nil)
    (dispatch conn :player/create :npc)
    (let [id (:db/id (scene-token conn))
          player-id (:db/id (first (root-players conn)))]
      (add-conn! conn my-uuid)
      (dispatch conn :objects/assign-owner [id] player-id)
      (dispatch conn :player/set-controller player-id [:user/uuid my-uuid])
      (dispatch conn :objects/toggle-hidden id)
      (is (:object/hidden (entity @conn id))
          "the assigned, connected controller may toggle it even though
           they aren't the host"))))

(deftest test-objects-toggle-hidden-unrelated-guest-noop
  (let [conn (ds/conn-from-db (initial-data false))
        my-uuid (random-uuid)
        other-uuid (random-uuid)]
    (transact! conn [{:db/id [:db/ident :user] :user/uuid my-uuid}])
    (dispatch conn :token/create (Vec2. 0 0) nil)
    (dispatch conn :player/create :npc)
    (let [id (:db/id (scene-token conn))
          player-id (:db/id (first (root-players conn)))]
      (add-conn! conn other-uuid)
      (dispatch conn :objects/assign-owner [id] player-id)
      (dispatch conn :player/set-controller player-id [:user/uuid other-uuid])
      (dispatch conn :objects/toggle-hidden id)
      (is (not (:object/hidden (entity @conn id)))
          "a connected guest who isn't the assigned controller (and isn't
           the host) may not toggle it"))))

(deftest test-objects-toggle-hidden-stale-controller-falls-back-to-host
  (let [conn (ds/conn-from-db (initial-data true))
        stale-uuid (random-uuid)]
    (dispatch conn :token/create (Vec2. 0 0) nil)
    (dispatch conn :player/create :npc)
    (let [id (:db/id (scene-token conn))
          player-id (:db/id (first (root-players conn)))]
      (add-conn! conn stale-uuid)
      (dispatch conn :objects/assign-owner [id] player-id)
      (dispatch conn :player/set-controller player-id [:user/uuid stale-uuid])
      ;; simulate the controlling guest disconnecting -- retracted from
      ;; :session/conns, but :player/controller still points at them
      (transact! conn [[:db/retract [:db/ident :session] :session/conns [:user/uuid stale-uuid]]])
      (dispatch conn :objects/toggle-hidden id)
      (is (:object/hidden (entity @conn id))
          "a controller ref pointing at someone no longer connected falls
           back to host-only, same as an unassigned object"))))

;; --- Dice ---
(defn ^:private scene-dice-rolls [conn]
  (:scene/dice-rolls (current-scene conn)))

(deftest test-dice-roll-neutral
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :dice/roll [20 6 6] nil nil)
    (let [roll (first (scene-dice-rolls conn))]
      (is (= (count (scene-dice-rolls conn)) 1)
          "one new roll entity, whatever the pool size")
      (is (= (count (:roll/dice roll)) 3) "one result per die in the pool")
      (is (nil? (:roll/owner roll)) "no owner given, no D&D element enabled -- stays neutral")
      (is (nil? (:roll/mode roll)) "no mode given -- a plain summed pool")
      (is (= (:roll/result roll) (dice/sum (:roll/dice roll)))
          "the stored result is the sum of the ACTUAL stored dice, not a fixed number"))))

(deftest test-dice-roll-empty-pool-noop
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :dice/roll [] nil nil)
    (is (empty? (scene-dice-rolls conn)) "nothing to roll, nothing is created")))

(deftest test-dice-roll-mode-and-owner-dropped-without-dnd-element
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :player/create :npc)
    (let [player-id (:db/id (first (root-players conn)))]
      (dispatch conn :dice/roll [20 20] :advantage player-id)
      (let [roll (first (scene-dice-rolls conn))]
        (is (nil? (:roll/mode roll))
            ":dnd5e/dice-roller isn't enabled -- advantage is silently dropped")
        (is (nil? (:roll/owner roll))
            ":dnd5e/dice-roller isn't enabled -- the owner is silently dropped too")
        (is (= (:roll/result roll) (dice/sum (:roll/dice roll)))
            "downgraded all the way to a plain summed roll, not rejected outright")))))

(deftest test-dice-roll-advantage
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:dnd5e/dice-roller})
    (dispatch conn :dice/roll [20 20] :advantage nil)
    (let [roll (first (scene-dice-rolls conn))]
      (is (= (:roll/mode roll) :advantage))
      (is (= (:roll/result roll) (:value (dice/best (:roll/dice roll))))
          "the result is the best of the actual stored rolls, not the sum"))))

(deftest test-dice-roll-disadvantage
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:dnd5e/dice-roller})
    (dispatch conn :dice/roll [20 20] :disadvantage nil)
    (let [roll (first (scene-dice-rolls conn))]
      (is (= (:roll/mode roll) :disadvantage))
      (is (= (:roll/result roll) (:value (dice/worst (:roll/dice roll))))
          "the result is the worst of the actual stored rolls, not the sum"))))

(deftest test-dice-roll-owner-rejected-without-authority
  (let [conn (ds/conn-from-db (initial-data true))
        guest-uuid (random-uuid)]
    (set-enabled-elements! conn #{:dnd5e/dice-roller})
    (dispatch conn :player/create :npc)
    (let [player-id (:db/id (first (root-players conn)))]
      (add-conn! conn guest-uuid)
      (dispatch conn :player/set-controller player-id [:user/uuid guest-uuid])
      (dispatch conn :dice/roll [20] nil player-id)
      (is (empty? (scene-dice-rolls conn))
          "the host has no authority over a connected, assigned controller's
           seat -- the whole dispatch no-ops, mirroring :go-fish/score's own
           unauthorized-seat rejection, not just a dropped owner"))))

(deftest test-dice-roll-owner-accepted-self-controlled
  (let [conn (ds/conn-from-db (initial-data false))
        my-uuid (random-uuid)]
    (transact! conn [{:db/id [:db/ident :user] :user/uuid my-uuid}])
    (set-enabled-elements! conn #{:dnd5e/dice-roller})
    (dispatch conn :player/create :npc)
    (let [player-id (:db/id (first (root-players conn)))]
      (add-conn! conn my-uuid)
      (dispatch conn :player/set-controller player-id [:user/uuid my-uuid])
      (dispatch conn :dice/roll [20] nil player-id)
      (let [roll (first (scene-dice-rolls conn))]
        (is (= (:db/id (:roll/owner roll)) player-id)
            "a connected player rolling as the seat they themselves control succeeds")))))

(deftest test-dice-roll-owner-accepted-host-fallback
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:dnd5e/dice-roller})
    (dispatch conn :player/create :npc)
    (let [player-id (:db/id (first (root-players conn)))]
      (dispatch conn :dice/roll [20] nil player-id)
      (let [roll (first (scene-dice-rolls conn))]
        (is (= (:db/id (:roll/owner roll)) player-id)
            "an unassigned seat has no connected controller -- authority falls
             back to the host, same as :objects/toggle-hidden's own fallback")))))

(deftest test-dice-clear
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :dice/roll [20] nil nil)
    (dispatch conn :dice/roll [6 6] nil nil)
    (is (= (count (scene-dice-rolls conn)) 2))
    (dispatch conn :dice/clear)
    (is (empty? (scene-dice-rolls conn)) "every roll entity is retracted")))

;; --- Attack Modifier Decks (Gloomhaven-family "x-haven") ---
(defn ^:private scene-attack-decks [conn]
  (:scene/attack-decks (current-scene conn)))

(defn ^:private scene-attack-draws [conn]
  (:scene/attack-draws (current-scene conn)))

(defn ^:private force-attack-deck-top!
  "Test helper: relocates one :draw-pile card of `kind` in `deck` to the
   highest :card/position (i.e. 'top of the pile') so the next draw is
   deterministic."
  [conn deck kind]
  (let [draw (filter (comp #{:draw} :card/location) (:deck/cards deck))
        card (first (filter (comp #{kind} :card/rank) draw))
        max-pos (apply max (map :card/position draw))]
    (transact! conn [{:db/id (:db/id card) :card/position (inc max-pos)}])
    (:db/id card)))

(defn ^:private force-attack-deck-top-two!
  "Test helper: positions two DISTINCT :draw-pile cards (`kind-a` then
   `kind-b`, kind-a on top) as the top two of `deck`'s draw pile, for
   deterministic Advantage/Disadvantage tests."
  [conn deck kind-a kind-b]
  (let [draw (filter (comp #{:draw} :card/location) (:deck/cards deck))
        card-a (first (filter (comp #{kind-a} :card/rank) draw))
        remaining (remove (comp #{(:db/id card-a)} :db/id) draw)
        card-b (first (filter (comp #{kind-b} :card/rank) remaining))
        max-pos (apply max (map :card/position draw))]
    (transact! conn [{:db/id (:db/id card-b) :card/position (inc max-pos)}
                      {:db/id (:db/id card-a) :card/position (+ max-pos 2)}])))

(deftest test-attack-deck-create-personal-and-monster
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :player/create :npc)
    (let [player-id (:db/id (first (root-players conn)))]
      (dispatch conn :attack-deck/create player-id)
      (dispatch conn :attack-deck/create nil)
      (let [decks (scene-attack-decks conn)
            personal (first (filter (comp #{player-id} :db/id :deck/owner) decks))
            monster (first (filter (comp nil? :deck/owner) decks))]
        (is (= (count decks) 2))
        (is (= (count (:deck/cards personal)) 20) "a fresh standard 20-card deck")
        (is (= (:deck/name personal) (:player/name (first (root-players conn)))))
        (is (= (:deck/name monster) "Monsters"))
        (is (every? (comp #{:draw} :card/location) (:deck/cards personal))
            "every card starts in the draw pile")))))

(deftest test-attack-deck-create-no-op-when-owner-already-has-one
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :attack-deck/create nil)
    (dispatch conn :attack-deck/create nil)
    (is (= (count (scene-attack-decks conn)) 1) "only one monster deck ever exists")
    (dispatch conn :player/create :npc)
    (let [player-id (:db/id (first (root-players conn)))]
      (dispatch conn :attack-deck/create player-id)
      (dispatch conn :attack-deck/create player-id)
      (is (= (count (scene-attack-decks conn)) 2) "only one personal deck per player"))))

(deftest test-attack-deck-create-noop-without-element-enabled
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :attack-deck/create nil)
    (is (empty? (scene-attack-decks conn))
        ":gloomhaven/attack-deck isn't enabled -- the whole dispatch no-ops")))

(deftest test-attack-deck-draw-neutral
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :attack-deck/create nil)
    (let [deck-id (:db/id (first (scene-attack-decks conn)))]
      (force-attack-deck-top! conn (entity @conn deck-id) :plus-1)
      (dispatch conn :attack-deck/draw deck-id nil)
      (let [deck (entity @conn deck-id)
            draw (first (scene-attack-draws conn))]
        (is (= (count (scene-attack-draws conn)) 1))
        (is (= (:draw/kind draw) :plus-1))
        (is (nil? (:draw/mode draw)))
        (is (nil? (:draw/discarded-kind draw)))
        (is (= (count (filter (comp #{:draw} :card/location) (:deck/cards deck))) 19))
        (is (= (count (filter (comp #{:discard} :card/location) (:deck/cards deck))) 1)
            "a plain numbered card moves to discard, it isn't removed")))))

(deftest test-attack-deck-draw-bless-removed-not-discarded
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :attack-deck/create nil)
    (let [deck-id (:db/id (first (scene-attack-decks conn)))]
      (dispatch conn :attack-deck/add-bless deck-id 1)
      (force-attack-deck-top! conn (entity @conn deck-id) :bless)
      (dispatch conn :attack-deck/draw deck-id nil)
      (let [deck (entity @conn deck-id)
            draw (first (scene-attack-draws conn))]
        (is (= (:draw/kind draw) :bless))
        (is (= (count (:deck/cards deck)) 20)
            "BLESS is removed from the deck entirely when drawn, not discarded --
             back down to the original 20 (21 after add-bless, minus 1 drawn)")
        (is (empty? (filter (comp #{:discard} :card/location) (:deck/cards deck)))
            "nothing landed in discard")))))

(deftest test-attack-deck-draw-flips-needs-reshuffle
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :attack-deck/create nil)
    (let [deck-id (:db/id (first (scene-attack-decks conn)))]
      (force-attack-deck-top! conn (entity @conn deck-id) :times-2)
      (dispatch conn :attack-deck/draw deck-id nil)
      (is (:deck/needs-reshuffle? (entity @conn deck-id))
          "drawing the 2x card flags the deck for an end-of-round reshuffle"))))

(deftest test-attack-deck-draw-plain-card-leaves-needs-reshuffle-false
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :attack-deck/create nil)
    (let [deck-id (:db/id (first (scene-attack-decks conn)))]
      (force-attack-deck-top! conn (entity @conn deck-id) :plus-0)
      (dispatch conn :attack-deck/draw deck-id nil)
      (is (not (:deck/needs-reshuffle? (entity @conn deck-id)))))))

(deftest test-attack-deck-draw-reactive-reshuffle-when-draw-pile-empty
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :attack-deck/create nil)
    (let [deck-id (:db/id (first (scene-attack-decks conn)))
          deck (entity @conn deck-id)]
      (transact! conn (map-indexed (fn [i c] {:db/id (:db/id c) :card/location :discard :card/position i})
                                    (:deck/cards deck)))
      (dispatch conn :attack-deck/draw deck-id nil)
      (let [after (entity @conn deck-id)]
        (is (= (count (scene-attack-draws conn)) 1)
            "the empty draw pile reshuffled from discard reactively, mid-draw")
        (is (= (count (filter (comp #{:draw} :card/location) (:deck/cards after))) 19))
        (is (= (count (filter (comp #{:discard} :card/location) (:deck/cards after))) 1))))))

(deftest test-attack-deck-draw-empty-deck-noop
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :attack-deck/create nil)
    (let [deck-id (:db/id (first (scene-attack-decks conn)))]
      (doseq [kind [:minus-2 :minus-1 :plus-0 :plus-1 :plus-2 :null :times-2]]
        (dispatch conn :attack-deck/remove-cards deck-id kind 10))
      (is (empty? (:deck/cards (entity @conn deck-id))) "deck fully emptied")
      (dispatch conn :attack-deck/draw deck-id nil)
      (is (empty? (scene-attack-draws conn)) "nothing to draw, nothing recorded"))))

(deftest test-attack-deck-advantage-keeps-better
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :attack-deck/create nil)
    (let [deck-id (:db/id (first (scene-attack-decks conn)))]
      (force-attack-deck-top-two! conn (entity @conn deck-id) :minus-1 :plus-2)
      (dispatch conn :attack-deck/draw deck-id :advantage)
      (let [draw (first (scene-attack-draws conn))
            deck (entity @conn deck-id)]
        (is (= (:draw/mode draw) :advantage))
        (is (= (:draw/kind draw) :plus-2) "the numerically better of the two drawn cards")
        (is (= (:draw/discarded-kind draw) :minus-1))
        (is (= (count (filter (comp #{:discard} :card/location) (:deck/cards deck))) 2)
            "both drawn cards move to discard -- the 'losing' one isn't retracted")))))

(deftest test-attack-deck-disadvantage-keeps-worse
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :attack-deck/create nil)
    (let [deck-id (:db/id (first (scene-attack-decks conn)))]
      (force-attack-deck-top-two! conn (entity @conn deck-id) :plus-2 :minus-1)
      (dispatch conn :attack-deck/draw deck-id :disadvantage)
      (let [draw (first (scene-attack-draws conn))]
        (is (= (:draw/mode draw) :disadvantage))
        (is (= (:draw/kind draw) :minus-1) "the numerically worse of the two drawn cards")
        (is (= (:draw/discarded-kind draw) :plus-2))))))

(deftest test-attack-deck-advantage-flags-reshuffle-on-either-card
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :attack-deck/create nil)
    (let [deck-id (:db/id (first (scene-attack-decks conn)))]
      (force-attack-deck-top-two! conn (entity @conn deck-id) :null :plus-1)
      (dispatch conn :attack-deck/draw deck-id :advantage)
      (let [draw (first (scene-attack-draws conn))]
        (is (= (:draw/kind draw) :plus-1) "Null always loses the comparison, plus-1 is kept")
        (is (:deck/needs-reshuffle? (entity @conn deck-id))
            "the discarded card was Null -- still flags the deck, not just the applied one")))))

(deftest test-attack-deck-add-cards
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :attack-deck/create nil)
    (let [deck-id (:db/id (first (scene-attack-decks conn)))]
      (dispatch conn :attack-deck/add-cards deck-id :plus-1 2)
      (let [deck (entity @conn deck-id)]
        (is (= (count (:deck/cards deck)) 22))
        (is (= (count (filter (comp #{:plus-1} :card/rank) (:deck/cards deck))) 7)
            "5 standard + 2 added")
        (is (every? (comp #{:draw} :card/location) (:deck/cards deck))
            "new cards join the draw pile")))))

(deftest test-attack-deck-remove-cards
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :attack-deck/create nil)
    (let [deck-id (:db/id (first (scene-attack-decks conn)))]
      (dispatch conn :attack-deck/remove-cards deck-id :minus-1 2)
      (let [deck (entity @conn deck-id)]
        (is (= (count (:deck/cards deck)) 18))
        (is (= (count (filter (comp #{:minus-1} :card/rank) (:deck/cards deck))) 3)
            "5 standard minus 2 removed"))
      (dispatch conn :attack-deck/remove-cards deck-id :minus-2 10)
      (is (= (count (filter (comp #{:minus-2} :card/rank) (:deck/cards (entity @conn deck-id)))) 0)
          "removing more than exist just removes as many as are actually there"))))

(deftest test-attack-deck-replace-card
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :attack-deck/create nil)
    (let [deck-id (:db/id (first (scene-attack-decks conn)))]
      (dispatch conn :attack-deck/replace-card deck-id :minus-2 :minus-1)
      (let [deck (entity @conn deck-id)]
        (is (= (count (:deck/cards deck)) 20) "one swapped for one, total unchanged")
        (is (= (count (filter (comp #{:minus-2} :card/rank) (:deck/cards deck))) 0))
        (is (= (count (filter (comp #{:minus-1} :card/rank) (:deck/cards deck))) 6)))
      (dispatch conn :attack-deck/replace-card deck-id :minus-2 :plus-1)
      (is (= (count (:deck/cards (entity @conn deck-id))) 20)
          "no -2 cards left to replace -- a no-op, not an error"))))

(deftest test-attack-deck-add-bless-and-curse
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :attack-deck/create nil)
    (let [deck-id (:db/id (first (scene-attack-decks conn)))]
      (dispatch conn :attack-deck/add-bless deck-id 1)
      (dispatch conn :attack-deck/add-curse deck-id 2)
      (let [deck (entity @conn deck-id)]
        (is (= (count (:deck/cards deck)) 23))
        (is (= (count (filter (comp #{:bless} :card/rank) (:deck/cards deck))) 1))
        (is (= (count (filter (comp #{:curse} :card/rank) (:deck/cards deck))) 2))))))

(deftest test-attack-deck-reshuffle
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :attack-deck/create nil)
    (let [deck-id (:db/id (first (scene-attack-decks conn)))]
      (force-attack-deck-top! conn (entity @conn deck-id) :null)
      (dispatch conn :attack-deck/draw deck-id nil)
      (is (:deck/needs-reshuffle? (entity @conn deck-id)))
      (dispatch conn :attack-deck/reshuffle deck-id)
      (let [deck (entity @conn deck-id)]
        (is (not (:deck/needs-reshuffle? deck)))
        (is (= (count (filter (comp #{:draw} :card/location) (:deck/cards deck))) 20)
            "everything is back in the draw pile")))))

(deftest test-attack-deck-reshuffle-flagged-only-sweeps-flagged-decks
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :attack-deck/create nil)
    (dispatch conn :player/create :npc)
    (let [player-id (:db/id (first (root-players conn)))]
      (dispatch conn :attack-deck/create player-id)
      (let [decks (scene-attack-decks conn)
            monster-id (:db/id (first (filter (comp nil? :deck/owner) decks)))
            player-deck-id (:db/id (first (filter (comp some? :deck/owner) decks)))]
        (force-attack-deck-top! conn (entity @conn monster-id) :times-2)
        (dispatch conn :attack-deck/draw monster-id nil)
        (dispatch conn :attack-deck/reshuffle-flagged)
        (is (not (:deck/needs-reshuffle? (entity @conn monster-id))))
        (is (empty? (filter (comp #{:discard} :card/location) (:deck/cards (entity @conn player-deck-id))))
            "the untouched player deck was never drawn from -- nothing to reshuffle")))))

(deftest test-attack-deck-reshuffle-flagged-host-only
  (let [conn (ds/conn-from-db (initial-data false))
        my-uuid (random-uuid)]
    (transact! conn [{:db/id [:db/ident :user] :user/uuid my-uuid}])
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :player/create :npc)
    (let [player-id (:db/id (first (root-players conn)))]
      (add-conn! conn my-uuid)
      (dispatch conn :player/set-controller player-id [:user/uuid my-uuid])
      ;; a self-controlled PERSONAL deck, unlike the monster deck, is
      ;; something this non-host guest may create/draw from -- the point
      ;; of this test is that :attack-deck/reshuffle-flagged is host-only
      ;; regardless, even over a deck the guest otherwise has full
      ;; authority over.
      (dispatch conn :attack-deck/create player-id)
      (let [deck-id (:db/id (first (scene-attack-decks conn)))]
        (force-attack-deck-top! conn (entity @conn deck-id) :null)
        (dispatch conn :attack-deck/draw deck-id nil)
        (dispatch conn :attack-deck/reshuffle-flagged)
        (is (:deck/needs-reshuffle? (entity @conn deck-id))
            "a non-host guest may not sweep the scene's decks, even one they
             themselves fully control")))))

(deftest test-attack-deck-reset
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :attack-deck/create nil)
    (let [deck-id (:db/id (first (scene-attack-decks conn)))]
      (dispatch conn :attack-deck/add-cards deck-id :plus-1 3)
      (is (= (count (:deck/cards (entity @conn deck-id))) 23))
      (dispatch conn :attack-deck/reset deck-id)
      (let [deck (entity @conn deck-id)]
        (is (= (count (:deck/cards deck)) 20) "back to a fresh 20-card standard composition")
        (is (every? (comp #{:draw} :card/location) (:deck/cards deck)))
        (is (not (:deck/needs-reshuffle? deck)))))))

(deftest test-attack-deck-remove
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :attack-deck/create nil)
    (let [deck-id (:db/id (first (scene-attack-decks conn)))
          card-id (:db/id (first (:deck/cards (entity @conn deck-id))))]
      (dispatch conn :attack-deck/remove deck-id)
      (is (nil? (:db/id (entity @conn deck-id))))
      (is (nil? (:card/location (entity @conn card-id))) "its cards were retracted too")
      (is (empty? (scene-attack-decks conn))))))

(deftest test-attack-deck-draw-owner-rejected-without-authority
  (let [conn (ds/conn-from-db (initial-data true))
        guest-uuid (random-uuid)]
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :player/create :npc)
    (let [player-id (:db/id (first (root-players conn)))]
      (add-conn! conn guest-uuid)
      (dispatch conn :player/set-controller player-id [:user/uuid guest-uuid])
      (dispatch conn :attack-deck/create player-id)
      (is (empty? (scene-attack-decks conn))
          "the host has no authority over a connected, assigned controller's seat --
           the whole dispatch no-ops, mirroring :dice/roll's own unauthorized-owner
           rejection"))))

(deftest test-attack-deck-monster-deck-host-only
  (let [conn (ds/conn-from-db (initial-data false))
        my-uuid (random-uuid)]
    (transact! conn [{:db/id [:db/ident :user] :user/uuid my-uuid}])
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (add-conn! conn my-uuid)
    (dispatch conn :attack-deck/create nil)
    (is (empty? (scene-attack-decks conn))
        "a non-host connected guest may not create/act on the shared monster deck")))

(deftest test-attack-deck-personal-deck-self-controlled-accepted
  (let [conn (ds/conn-from-db (initial-data false))
        my-uuid (random-uuid)]
    (transact! conn [{:db/id [:db/ident :user] :user/uuid my-uuid}])
    (set-enabled-elements! conn #{:gloomhaven/attack-deck})
    (dispatch conn :player/create :npc)
    (let [player-id (:db/id (first (root-players conn)))]
      (add-conn! conn my-uuid)
      (dispatch conn :player/set-controller player-id [:user/uuid my-uuid])
      (dispatch conn :attack-deck/create player-id)
      (is (= (count (scene-attack-decks conn)) 1)
          "a connected player may create/act on the deck for the seat they
           themselves control, even though they aren't the host"))))

;; --- Prop variables, copies, and physical piles ---
(defn ^:private scene-props [conn]
  (:scene/props (:camera/scene (:user/camera (user conn)))))

(deftest test-objects-merge-variables
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :token/create (Vec2. 0 0) nil)
    (let [id (:db/id (scene-token conn))]
      (dispatch conn :objects/merge-variables [id] {:a 1 :b 2})
      (is (= (:object/variables (entity @conn id)) {:a 1 :b 2}))
      (dispatch conn :objects/merge-variables [id] {:a nil})
      (is (= (:object/variables (entity @conn id)) {:b 2})
          "a nil-valued key in the merge removes just that key, not the
           whole attribute")
      (dispatch conn :objects/merge-variables [id] {:b nil})
      (is (nil? (:object/variables (entity @conn id)))
          "clearing the last key retracts the whole attribute rather
           than leaving an empty map behind"))))

(deftest test-objects-assign-shared
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :token/create (Vec2. 0 0) nil)
    (let [id (:db/id (scene-token conn))]
      (dispatch conn :objects/assign-shared [id] true)
      (is (:object/shared? (entity @conn id)))
      (dispatch conn :objects/assign-shared [id] false)
      (is (nil? (:object/shared? (entity @conn id)))
          "clearing retracts the flag rather than storing false"))))

(defn ^:private seed-props-image!
  "Test helper -- uploads a single fake prop image under `hash`, the
   same real precondition :props/create/:props/create-many always have
   in actual UI usage (a prop is only ever placed from an already-
   uploaded gallery entry; :props/create-many's contract is identical to
   :props/create's, it just wasn't exercised with a fresh hash by any
   test until this one). Mirrors how test-objects-assign-alt-image
   already seeds 'abc' via :token-images/create-many before referencing
   it."
  [conn hash]
  (dispatch conn :props-images/create-many
            [[{:hash hash :name hash :size 1 :width 1 :height 1}
              {:hash hash :name hash :size 1 :width 1 :height 1}]]))

(deftest test-props-create-many-stack-layout
  (let [conn (ds/conn-from-db (initial-data true))]
    (seed-props-image! conn "card-hash")
    (dispatch conn :props/create-many (Vec2. 5 5) "card-hash" 3)
    (let [props (scene-props conn)]
      (is (= (count props) 3))
      (is (every? #(= (:object/point %) (Vec2. 5 5)) props)
          "every copy lands on the exact same point -- a stack, the
           default layout")
      (is (= (set (map (comp :prop/copy-index :object/variables) props)) #{0 1 2})
          "each copy gets a distinct, automatic identifier"))))

(deftest test-props-create-many-grid-layout
  (let [conn (ds/conn-from-db (initial-data true))]
    (seed-props-image! conn "card-hash")
    (dispatch conn :props/create-many (Vec2. 0 0) "card-hash" 4
              {:layout :grid :columns 2 :spacing 10})
    (let [points (into #{} (map :object/point) (scene-props conn))]
      (is (= points #{(Vec2. 0 0) (Vec2. 10 0) (Vec2. 0 10) (Vec2. 10 10)})
          "row-major spread, 2 columns wide, 10 units apart"))))

(deftest test-props-create-many-mint-time-options
  (let [conn (ds/conn-from-db (initial-data true))]
    (seed-props-image! conn "card-back")
    (seed-props-image! conn "card-front")
    (dispatch conn :props/create-many (Vec2. 0 0) "card-back" 1
              {:hidden? true :shared? true :alt-hash "card-front"})
    (let [prop (first (scene-props conn))]
      (is (:object/hidden prop))
      (is (:object/shared? prop))
      (is (= (:image/hash (:prop/image-alt prop)) "card-front")))))

(deftest test-props-create-pile
  (let [conn (ds/conn-from-db (initial-data true))]
    (seed-props-image! conn "card-back")
    (dispatch conn :props/create-pile (Vec2. 0 0) "card-back" 4 "pile-1")
    (let [vars (map :object/variables (scene-props conn))]
      (is (= (into #{} (map :pile/id) vars) #{"pile-1"}))
      (is (= (into #{} (map :pile/position) vars) #{0 1 2 3})))))

(deftest test-props-draw-from-pile-moves-top-and-unpiles
  (let [conn (ds/conn-from-db (initial-data true))]
    (seed-props-image! conn "card-back")
    (dispatch conn :props/create-pile (Vec2. 0 0) "card-back" 3 "pile-1")
    (let [top-id (:db/id (props/top-of-pile (scene-props conn)))
          target (Vec2. 40 40)]
      (dispatch conn :props/draw-from-pile "pile-1" target)
      (let [drawn (entity @conn top-id)]
        (is (= (:object/point drawn) target))
        (is (nil? (:pile/id (:object/variables drawn)))
            "un-piled -- no longer a member of pile-1")
        (is (= (:prop/copy-index (:object/variables drawn)) 2)
            "other variables (the automatic copy-index) are preserved")
        (is (= (count (props/pile (scene-props conn) "pile-1")) 2)
            "the pile itself shrinks by one")))))

(deftest test-props-draw-from-pile-in-place-when-no-target
  (let [conn (ds/conn-from-db (initial-data true))]
    (seed-props-image! conn "card-back")
    (dispatch conn :props/create-pile (Vec2. 7 7) "card-back" 2 "pile-1")
    (let [top-id (:db/id (props/top-of-pile (scene-props conn)))]
      (dispatch conn :props/draw-from-pile "pile-1" nil)
      (is (= (:object/point (entity @conn top-id)) (Vec2. 7 7))
          "no target-point -- un-piles without moving it"))))

(deftest test-props-draw-from-pile-noop-when-empty
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :props/draw-from-pile "no-such-pile" (Vec2. 1 1))
    (is (empty? (scene-props conn))
        "no matching pile-id is a no-op, not an error")))

(deftest test-props-discard-to-pile-restacks-onto-existing-point
  (let [conn (ds/conn-from-db (initial-data true))]
    (seed-props-image! conn "card-back")
    (dispatch conn :props/create-pile (Vec2. 3 3) "card-back" 2 "pile-1")
    (dispatch conn :props/create-many (Vec2. 99 99) "card-back" 1) ; a loose, un-piled prop
    (let [loose-id (:db/id (first (filter #(nil? (:pile/id (:object/variables %)))
                                           (scene-props conn))))]
      (dispatch conn :props/discard-to-pile loose-id "pile-1")
      (let [discarded (entity @conn loose-id)]
        (is (= (:object/point discarded) (Vec2. 3 3))
            "relocated onto the pile's existing anchor point")
        (is (= (:pile/id (:object/variables discarded)) "pile-1"))
        (is (= (:pile/position (:object/variables discarded)) 2)
            "lands on top -- one past the existing max position")))))

(deftest test-objects-toggle-hidden-shared-guest-allowed
  (let [conn (ds/conn-from-db (initial-data false))
        my-uuid (random-uuid)]
    (transact! conn [{:db/id [:db/ident :user] :user/uuid my-uuid}])
    (dispatch conn :token/create (Vec2. 0 0) nil)
    (let [id (:db/id (scene-token conn))]
      (add-conn! conn my-uuid)
      (dispatch conn :objects/assign-shared [id] true)
      (dispatch conn :objects/toggle-hidden id)
      (is (:object/hidden (entity @conn id))
          "an unrelated connected guest -- not the host, no owner/
           controller assigned at all -- may still toggle it purely
           because :object/shared? is true"))))

;; --- Memory (example game) ---
;; The second game ported onto the generic mini-game session
;; scaffolding (see events.cljs's 'Mini-game sessions' section) -- a
;; session-scoped prototype for letting several independent, arbitrary-
;; subset-of-the-roster tables run nested inside one scene at once.
(defn ^:private scene-memory [conn]
  (:camera/scene (:user/camera (user conn))))

(defn ^:private memory-sessions [conn]
  (filter (comp #{:memory} :minigame/kind) (:scene/minigames (scene-memory conn))))

(defn ^:private memory-session [conn]
  (first (memory-sessions conn)))

(defn ^:private memory-cards [minigame]
  (:minigame/props minigame))

(defn ^:private memory-players
  [minigame]
  (mapv (comp :db/id :seat/player) (sort-by :seat/order (:minigame/seats minigame))))

(deftest test-memory-start
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:memory/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :memory/start ids)
      (let [minigame (memory-session conn)
            cards (memory-cards minigame)
            players (memory-players minigame)]
        (is (= (count players) 3))
        (is (= (set players) (set ids))
            "the turn cycle is exactly the participants given to
             :memory/start")
        (is (= (:minigame/turn-index minigame) 0))
        (is (= (:minigame/label minigame) "Memory 1"))
        (is (nil? (:minigame/scores minigame)))
        (is (empty? (:scene/decks (scene-memory conn)))
            "Memory has no deck at all -- nothing lands in :scene/decks")
        (is (= (count cards) 44))
        (is (every? :object/hidden cards) "every card deals face-down")
        (is (every? :object/shared? cards) "every card is a public toggle")
        (is (= (frequencies (map (comp :memory/value :object/variables) cards))
               (into {} (map (fn [v] [v 2])) (range 22)))
            "22 values, exactly 2 copies each")))))

(deftest test-memory-start-rejected-without-element-enabled
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :memory/start ids)
      (is (empty? (memory-sessions conn))
          "rejected -- :memory/start checks :memory/game is actually
           enabled on the scene's own game-type"))))

(deftest test-memory-flip-turn-enforcement
  (let [conn (ds/conn-from-db (initial-data false))
        guest-uuid (random-uuid)]
    (transact! conn [{:db/id [:db/ident :user] :user/uuid guest-uuid}])
    (add-conn! conn guest-uuid)
    (set-enabled-elements! conn #{:memory/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :memory/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            second-player-id (second (memory-players (entity @conn minigame-id)))]
        (dispatch conn :player/set-controller second-player-id [:user/uuid guest-uuid])
        (let [card-id (:db/id (first (memory-cards (entity @conn minigame-id))))]
          (dispatch conn :memory/flip card-id)
          (is (:object/hidden (entity @conn card-id))
              "index 0's turn (unassigned -> host-only) -- this guest,
               mapped only to index 1, may not flip yet")
          ;; fast-forward to the 2nd player's turn for testing purposes
          (transact! conn [{:db/id minigame-id :minigame/turn-index 1}])
          (dispatch conn :memory/flip card-id)
          (is (not (:object/hidden (entity @conn card-id)))
              "index 1's turn -- this guest, as that player's controller,
               may flip"))))))

(deftest test-memory-flip-refuses-third-card
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:memory/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :memory/start ids)
      (let [[a b c] (take 3 (memory-cards (memory-session conn)))]
        (dispatch conn :memory/flip (:db/id a))
        (dispatch conn :memory/flip (:db/id b))
        (dispatch conn :memory/flip (:db/id c))
        (is (:object/hidden (entity @conn (:db/id c)))
            "a 3rd flip is refused while 2 cards are already face-up and
             awaiting :memory/resolve")))))

(deftest test-memory-flip-clears-user-dragging
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:memory/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :memory/start ids)
      (let [card-id (:db/id (first (memory-cards (memory-session conn))))]
        ;; a real onDragStart always precedes the click that triggers a
        ;; flip (use-drag-listener's onDragEnd zero-delta "click" case is
        ;; still a full drag-kit gesture) -- so :user/dragging is always
        ;; populated by the time :memory/flip runs.
        (dispatch conn :drag/start card-id)
        (is (seq (:user/dragging (user conn))) "sanity check")
        (dispatch conn :memory/flip card-id)
        (is (empty? (:user/dragging (user conn)))
            "a click-triggered flip must clear :user/dragging just like
             :objects/select does -- otherwise this peer's own stale
             entry (invisible to them locally, since :db/ident :user is
             peer-relative) permanently locks this card out of every
             OTHER peer's draggable registration, since
             scene_objects.cljs's dragging lock check reads every
             connection's :user/dragging except the viewer's own")))))

(deftest test-memory-resolve-match
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:memory/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :memory/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            by-value (group-by (comp :memory/value :object/variables) (memory-cards (entity @conn minigame-id)))
            [a b] (first (vals by-value))
            turn-id (first (memory-players (entity @conn minigame-id)))]
        (dispatch conn :memory/flip (:db/id a))
        (dispatch conn :memory/flip (:db/id b))
        (dispatch conn :memory/resolve minigame-id)
        (is (nil? (:db/id (entity @conn (:db/id a)))) "matched cards are retracted")
        (is (nil? (:db/id (entity @conn (:db/id b)))))
        (is (= (:minigame/scores (entity @conn minigame-id)) {turn-id 1})
            "the current-turn player's tally increments")
        (is (= (:minigame/turn-index (entity @conn minigame-id)) 0)
            "matching players go again -- turn index unchanged")))))

(deftest test-memory-resolve-mismatch
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:memory/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :memory/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            by-value (group-by (comp :memory/value :object/variables) (memory-cards (entity @conn minigame-id)))
            [va vb] (take 2 (keys by-value))
            a (first (by-value va))
            b (first (by-value vb))]
        (dispatch conn :memory/flip (:db/id a))
        (dispatch conn :memory/flip (:db/id b))
        (dispatch conn :memory/resolve minigame-id)
        (is (:object/hidden (entity @conn (:db/id a)))
            "mismatched cards are re-hidden, not removed")
        (is (:object/hidden (entity @conn (:db/id b))))
        (is (nil? (:minigame/scores (entity @conn minigame-id)))
            "no score change on a mismatch")
        (is (= (:minigame/turn-index (entity @conn minigame-id)) 1)
            "turn advances to the next player")))))

(deftest test-memory-resolve-noop-unless-two-face-up
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:memory/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :memory/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (dispatch conn :memory/resolve minigame-id)
        (is (= (count (memory-cards (entity @conn minigame-id))) 44)
            "no-op when nothing is face-up yet")))))

(deftest test-memory-two-simultaneous-sessions-dont-interfere
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:memory/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [[p1 p2 p3 p4] (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :memory/start [p1 p2])
      (let [session-a-id (minigame-viewing-id conn)]
        (dispatch conn :memory/start [p3 p4])
        (let [session-b-id (minigame-viewing-id conn)
              cards-a (set (map :db/id (memory-cards (entity @conn session-a-id))))
              cards-b (set (map :db/id (memory-cards (entity @conn session-b-id))))]
          (is (not= session-a-id session-b-id))
          (is (= (count (memory-sessions conn)) 2))
          (is (empty? (set/intersection cards-a cards-b))
              "each session owns its own independent set of cards")
          (let [by-value (group-by (comp :memory/value :object/variables) (memory-cards (entity @conn session-a-id)))
                [a b] (first (vals by-value))]
            (dispatch conn :memory/flip (:db/id a))
            (dispatch conn :memory/flip (:db/id b))
            (dispatch conn :memory/resolve session-a-id)
            (is (= (:minigame/turn-index (entity @conn session-a-id)) 0))
            (is (= (:minigame/turn-index (entity @conn session-b-id)) 0)
                "session B's turn index is completely unaffected by
                 session A's own resolve")))))))

(deftest test-memory-remove
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:memory/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :memory/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            card-ids (map :db/id (memory-cards (entity @conn minigame-id)))
            seat-ids (map :db/id (:minigame/seats (entity @conn minigame-id)))]
        (dispatch conn :minigame/remove minigame-id)
        (is (nil? (:db/id (entity @conn minigame-id))) "the session itself is retracted")
        (is (every? nil? (map (fn [id] (:db/id (entity @conn id))) card-ids))
            "every card is retracted too -- :minigame/props is a borrowed,
             NON-component ref, so :minigame/remove retracts them
             explicitly rather than relying on cascade")
        (is (every? nil? (map (fn [id] (:db/id (entity @conn id))) seat-ids)) "its seats are retracted too")
        (is (empty? (memory-sessions conn)))))))

;; --- Memory: mid-game roster reactivity ---
;; A session's seats stay a frozen seating order from :memory/start, but
;; "whose turn is it" is resolved live against current :player/active
;; state (ogres.app.turn-order/valid-turn-index) -- these confirm a
;; bench/remove mid-game takes effect immediately, without a page reload
;; or a separate correction event.
(deftest test-memory-flip-skips-benched-turn-player
  (let [conn (ds/conn-from-db (initial-data false))
        guest-uuid (random-uuid)]
    (transact! conn [{:db/id [:db/ident :user] :user/uuid guest-uuid}])
    (add-conn! conn guest-uuid)
    (set-enabled-elements! conn #{:memory/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :memory/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            [first-id second-id] (memory-players (entity @conn minigame-id))]
        (dispatch conn :player/set-controller second-id [:user/uuid guest-uuid])
        (dispatch conn :player/set-active first-id false)
        (let [card-id (:db/id (first (memory-cards (entity @conn minigame-id))))]
          (dispatch conn :memory/flip card-id)
          (is (not (:object/hidden (entity @conn card-id)))
              "the stored index still points at index 0, but that player
               is now benched -- the turn cycle skips forward to index 1's
               controller (this guest) live, with no separate event"))))))

(deftest test-memory-resolve-mismatch-skips-benched-player
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:memory/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :memory/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            second-id (second (memory-players (entity @conn minigame-id)))]
        (dispatch conn :player/set-active second-id false)
        (let [by-value (group-by (comp :memory/value :object/variables) (memory-cards (entity @conn minigame-id)))
              [va vb] (take 2 (keys by-value))
              a (first (by-value va))
              b (first (by-value vb))]
          (dispatch conn :memory/flip (:db/id a))
          (dispatch conn :memory/flip (:db/id b))
          (dispatch conn :memory/resolve minigame-id)
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 2)
              "index 1's player is benched -- the stored index skips
               straight to index 2 instead of landing on a benched seat"))))))

(deftest test-memory-resolve-mismatch-skips-removed-player
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:memory/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :memory/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            second-id (second (memory-players (entity @conn minigame-id)))]
        (dispatch conn :player/remove second-id)
        (let [by-value (group-by (comp :memory/value :object/variables) (memory-cards (entity @conn minigame-id)))
              [va vb] (take 2 (keys by-value))
              a (first (by-value va))
              b (first (by-value vb))]
          (dispatch conn :memory/flip (:db/id a))
          (dispatch conn :memory/flip (:db/id b))
          (dispatch conn :memory/resolve minigame-id)
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 2)
              "index 1's player was removed entirely -- treated the same
               as benched, the stored index skips to index 2"))))))

(deftest test-memory-turn-player-nil-when-all-benched
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:memory/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :memory/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        ;; both seated players benched at once -- not just one, since
        ;; :memory/start now requires >= 2 participants.
        (doseq [id (memory-players (entity @conn minigame-id))]
          (dispatch conn :player/set-active id false))
        (let [card-id (:db/id (first (memory-cards (entity @conn minigame-id))))]
          (dispatch conn :memory/flip card-id)
          (is (not (:object/hidden (entity @conn card-id)))
              "every seated player benched at once -- memory-turn-player
               degrades to nil rather than throwing, and
               memory-authorized-for-turn?'s existing turn-continuity
               fallback (the same one already used for an unassigned
               turn player) still lets the host act rather than
               soft-locking the game"))))))

;; --- Go Fish (example game) ---
;; The third game ported onto the generic mini-game session scaffolding
;; (see events.cljs's 'Mini-game sessions' section) -- a session-scoped
;; prototype for letting several independent, arbitrary-subset-of-the-
;; roster tables run nested inside one scene at once.
(defn ^:private scene-go-fish [conn]
  (:camera/scene (:user/camera (user conn))))

(defn ^:private go-fish-sessions [conn]
  (filter (comp #{:go-fish} :minigame/kind) (:scene/minigames (scene-go-fish conn))))

(defn ^:private go-fish-session [conn]
  (first (go-fish-sessions conn)))

(defn ^:private go-fish-deck [minigame]
  (:minigame/deck minigame))

(defn ^:private go-fish-hand [minigame holder-id]
  (cards/cards-of-holder (:deck/cards (go-fish-deck minigame)) holder-id))

(defn ^:private go-fish-players [minigame]
  (mapv (comp :db/id :seat/player) (sort-by :seat/order (:minigame/seats minigame))))

(defn ^:private move-cards!
  "Test helper: directly relocates `cards` into `holder-id`'s hand,
   bypassing :go-fish/start's random deal -- ask/score tests need a
   deterministic hand, not whatever the shuffle happened to produce."
  [conn holder-id cards]
  (transact! conn (for [c cards] {:db/id (:db/id c) :card/location :hand :card/holder holder-id})))

(deftest test-go-fish-start
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:go-fish/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :go-fish/start ids)
      (let [minigame (go-fish-session conn)
            deck (go-fish-deck minigame)
            players (go-fish-players minigame)]
        (is (= (count players) 2))
        (is (= (set players) (set ids))
            "the turn cycle is exactly the participants given to
             :go-fish/start")
        (is (= (:minigame/turn-index minigame) 0))
        (is (nil? (:minigame/scores minigame)))
        (is (:minigame/neutral-authority? minigame))
        (is (empty? (:scene/decks (scene-go-fish conn)))
            "the session's deck is NOT also added to :scene/decks")
        (is (= (:deck/name deck) "Go Fish (9 Ranks)"))
        (is (= (count (:deck/cards deck)) 36) "9 ranks x 4 copies")
        (is (= (count (by-location deck :hand)) 12) "6 cards dealt to each of 2 players")
        (is (= (count (by-location deck :draw)) 24))
        (doseq [id players]
          (is (= (count (go-fish-hand minigame id)) 6)))))))

(deftest test-go-fish-start-rejected-without-element-enabled
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :go-fish/start ids)
      (is (empty? (go-fish-sessions conn))
          "rejected -- :go-fish/start checks :go-fish/game is actually
           enabled on the scene's own game-type"))))

(deftest test-go-fish-ask-hit-transfers-cards-and-advances-turn
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:go-fish/game :go-fish/book-scoring})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :go-fish/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (go-fish-deck (entity @conn minigame-id)))
        (let [[asker-id target-id] (go-fish-players (entity @conn minigame-id))
              twos (filter (comp #{:two} :card/rank) (:deck/cards (go-fish-deck (entity @conn minigame-id))))]
          (move-cards! conn asker-id [(first twos)])
          (move-cards! conn target-id (rest twos))
          (dispatch conn :go-fish/ask minigame-id asker-id target-id :two)
          (is (= (count (go-fish-hand (entity @conn minigame-id) asker-id)) 4)
              "every matching card moved to the asker's hand")
          (is (empty? (go-fish-hand (entity @conn minigame-id) target-id)) "none left with the target")
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 1)
              "a hit advances the turn by default (extra-turn-on-hit off)"))))))

(deftest test-go-fish-ask-hit-grants-extra-turn-when-enabled
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:go-fish/game :go-fish/book-scoring :go-fish/extra-turn-on-hit})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :go-fish/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (go-fish-deck (entity @conn minigame-id)))
        (let [[asker-id target-id] (go-fish-players (entity @conn minigame-id))
              twos (filter (comp #{:two} :card/rank) (:deck/cards (go-fish-deck (entity @conn minigame-id))))]
          (move-cards! conn asker-id [(first twos)])
          (move-cards! conn target-id (rest twos))
          (dispatch conn :go-fish/ask minigame-id asker-id target-id :two)
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 0)
              "extra-turn-on-hit keeps the same player's turn after a hit"))))))

(deftest test-go-fish-ask-miss-draws-and-advances-turn
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:go-fish/game :go-fish/book-scoring})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :go-fish/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (go-fish-deck (entity @conn minigame-id)))
        (let [[asker-id target-id] (go-fish-players (entity @conn minigame-id))
              twos (filter (comp #{:two} :card/rank) (:deck/cards (go-fish-deck (entity @conn minigame-id))))]
          ;; asker holds a :two, target holds none -- guaranteed miss
          (move-cards! conn asker-id [(first twos)])
          (force-deck-top-of-draw! conn (go-fish-deck (entity @conn minigame-id)) :three)
          (let [before (count (go-fish-hand (entity @conn minigame-id) asker-id))]
            (dispatch conn :go-fish/ask minigame-id asker-id target-id :two)
            (is (= (count (go-fish-hand (entity @conn minigame-id) asker-id)) (inc before))
                "the asker drew one card from the pile")
            (is (= (:minigame/turn-index (entity @conn minigame-id)) 1)
                "a miss without a lucky draw always advances the turn")))))))

(deftest test-go-fish-ask-lucky-draw-grants-extra-turn-when-enabled
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:go-fish/game :go-fish/book-scoring :go-fish/extra-turn-on-lucky-draw})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :go-fish/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (go-fish-deck (entity @conn minigame-id)))
        (let [[asker-id target-id] (go-fish-players (entity @conn minigame-id))
              twos (filter (comp #{:two} :card/rank) (:deck/cards (go-fish-deck (entity @conn minigame-id))))]
          (move-cards! conn asker-id [(first twos)])
          (force-deck-top-of-draw! conn (go-fish-deck (entity @conn minigame-id)) :two)
          (dispatch conn :go-fish/ask minigame-id asker-id target-id :two)
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 0)
              "drawing the exact rank asked for keeps the turn when the rule is on"))))))

(deftest test-go-fish-ask-rejects-non-next-target-when-ask-anyone-off
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:go-fish/game :go-fish/book-scoring})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :go-fish/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (go-fish-deck (entity @conn minigame-id)))
        (let [[asker-id _mid-id third-id] (go-fish-players (entity @conn minigame-id))
              twos (filter (comp #{:two} :card/rank) (:deck/cards (go-fish-deck (entity @conn minigame-id))))]
          (move-cards! conn asker-id [(first twos)])
          (move-cards! conn third-id (rest twos))
          (dispatch conn :go-fish/ask minigame-id asker-id third-id :two)
          (is (= (count (go-fish-hand (entity @conn minigame-id) asker-id)) 1)
              "asking the 3rd seat instead of the next one is refused --
               the asker's own :two never leaves their hand, still just
               the 1 card placed there for setup")
          (is (= (count (go-fish-hand (entity @conn minigame-id) third-id)) 3)
              "third seat's cards are untouched too")
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 0) "no-op, turn unchanged"))))))

(deftest test-go-fish-ask-rejects-when-asker-lacks-rank
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:go-fish/game :go-fish/book-scoring})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :go-fish/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (go-fish-deck (entity @conn minigame-id)))
        (let [[asker-id target-id] (go-fish-players (entity @conn minigame-id))
              twos (filter (comp #{:two} :card/rank) (:deck/cards (go-fish-deck (entity @conn minigame-id))))]
          (move-cards! conn target-id twos)
          (dispatch conn :go-fish/ask minigame-id asker-id target-id :two)
          (is (= (count (go-fish-hand (entity @conn minigame-id) target-id)) 4)
              "asker never held a :two at all -- refused, nothing moves"))))))

(deftest test-go-fish-ask-by-non-participant-rejected
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:go-fish/game :go-fish/book-scoring})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [[p1 p2 outsider] (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :go-fish/start [p1 p2])
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (go-fish-deck (entity @conn minigame-id)))
        (dispatch conn :go-fish/ask minigame-id outsider p2 :two)
        (is (empty? (go-fish-hand (entity @conn minigame-id) p1)) "no-op -- p1 unaffected")
        (is (empty? (go-fish-hand (entity @conn minigame-id) p2)) "no-op -- p2 unaffected")
        (is (= (:minigame/turn-index (entity @conn minigame-id)) 0) "no-op, turn unchanged")))))

(deftest test-go-fish-score-book-mode
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:go-fish/game :go-fish/book-scoring})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :go-fish/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (go-fish-deck (entity @conn minigame-id)))
        (let [player-id (first (go-fish-players (entity @conn minigame-id)))
              twos (filter (comp #{:two} :card/rank) (:deck/cards (go-fish-deck (entity @conn minigame-id))))]
          (move-cards! conn player-id twos)
          (dispatch conn :go-fish/score minigame-id player-id :two)
          (is (empty? (go-fish-hand (entity @conn minigame-id) player-id))
              "all 4 moved to :scored -- go-fish-hand (now :card/location-
               filtered) no longer counts them as still 'in hand'")
          (is (= (:minigame/scores (entity @conn minigame-id)) {player-id 1})
              "a completed book is worth 1 point, not 2"))))))

(deftest test-go-fish-score-book-mode-refuses-incomplete-set
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:go-fish/game :go-fish/book-scoring})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :go-fish/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (go-fish-deck (entity @conn minigame-id)))
        (let [player-id (first (go-fish-players (entity @conn minigame-id)))
              twos (filter (comp #{:two} :card/rank) (:deck/cards (go-fish-deck (entity @conn minigame-id))))]
          (move-cards! conn player-id (take 3 twos))
          (dispatch conn :go-fish/score minigame-id player-id :two)
          (is (nil? (:minigame/scores (entity @conn minigame-id)))
              "3 of 4 isn't a complete book -- no-op"))))))

(deftest test-go-fish-score-pair-mode
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:go-fish/game :go-fish/pair-scoring})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :go-fish/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (go-fish-deck (entity @conn minigame-id)))
        (let [player-id (first (go-fish-players (entity @conn minigame-id)))
              twos (filter (comp #{:two} :card/rank) (:deck/cards (go-fish-deck (entity @conn minigame-id))))]
          (move-cards! conn player-id (take 3 twos))
          (dispatch conn :go-fish/score minigame-id player-id :two)
          (is (= (:minigame/scores (entity @conn minigame-id)) {player-id 1})
              "a 3-of-a-kind lays down 1 pair, worth 1 point")
          (is (= (count (filter (comp #{:scored} :card/location) (:deck/cards (go-fish-deck (entity @conn minigame-id))))) 2)
              "2 of the 3 twos are now scored (go-fish-hand itself no longer
               counts them as 'in hand' once scored, see cards-of-holder's
               :card/location filter)")
          (is (= (count (go-fish-hand (entity @conn minigame-id) player-id)) 1)
              "the odd 3rd card stays in hand, unscored"))))))

(deftest test-go-fish-score-pair-mode-then-more-of-the-same-rank-arrives
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:go-fish/game :go-fish/pair-scoring})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :go-fish/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (go-fish-deck (entity @conn minigame-id)))
        (let [player-id (first (go-fish-players (entity @conn minigame-id)))
              twos (filter (comp #{:two} :card/rank) (:deck/cards (go-fish-deck (entity @conn minigame-id))))]
          ;; Score a pair, leaving the odd 3rd two unscored in hand -- then
          ;; a 4th two arrives later (e.g. from a subsequent successful
          ;; ask). Scoring again must count ONLY the 2 currently-in-hand
          ;; twos (the odd 3rd plus the new 4th), never the 2 already-
          ;; scored ones -- this is exactly the bug cards-of-holder's
          ;; missing :card/location filter would cause: go-fish-hand
          ;; would wrongly still include the 2 already-scored twos (they
          ;; keep :card/holder to record credit), inflating the
          ;; rank-count to 4 and re-processing already-scored cards.
          (move-cards! conn player-id (take 3 twos))
          (dispatch conn :go-fish/score minigame-id player-id :two)
          (move-cards! conn player-id [(nth twos 3)])
          (dispatch conn :go-fish/score minigame-id player-id :two)
          (is (= (:minigame/scores (entity @conn minigame-id)) {player-id 2})
              "1 point for the first pair, 1 more for the second -- not a
               single inflated re-score of stale already-scored cards")
          (is (= (count (filter (comp #{:scored} :card/location) (:deck/cards (go-fish-deck (entity @conn minigame-id))))) 4)
              "all 4 twos are scored exactly once each, never touched twice")
          (is (empty? (go-fish-hand (entity @conn minigame-id) player-id))))))))

(deftest test-go-fish-remove
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:go-fish/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :go-fish/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            deck-id (:db/id (go-fish-deck (entity @conn minigame-id)))
            seat-ids (map :db/id (:minigame/seats (entity @conn minigame-id)))]
        (dispatch conn :minigame/remove minigame-id)
        (is (nil? (:db/id (entity @conn minigame-id))) "the session itself is retracted")
        (is (nil? (:db/id (entity @conn deck-id))) "the deck and its cards are retracted")
        (is (every? nil? (map (fn [id] (:db/id (entity @conn id))) seat-ids)) "its seats are retracted too")
        (is (empty? (go-fish-sessions conn)))))))

(deftest test-go-fish-leaves-scene-neutral-authority-untouched
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:go-fish/game})
    (dispatch conn :scene/toggle-neutral-authority true)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :go-fish/start ids)
      (is (:scene/neutral-authority? (scene-go-fish conn))
          "starting a table doesn't touch the scene-wide flag")
      (let [minigame-id (minigame-viewing-id conn)]
        (is (:minigame/neutral-authority? (entity @conn minigame-id))
            "the SESSION gets its own hand-visibility default instead")
        (dispatch conn :minigame/remove minigame-id)
        (is (:scene/neutral-authority? (scene-go-fish conn))
            "ending the table doesn't touch it either")))))

(deftest test-go-fish-turn-skips-benched-player
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:go-fish/game :go-fish/book-scoring})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :go-fish/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (go-fish-deck (entity @conn minigame-id)))
        (let [[asker-id second-id third-id] (go-fish-players (entity @conn minigame-id))
              twos (filter (comp #{:two} :card/rank) (:deck/cards (go-fish-deck (entity @conn minigame-id))))]
          (dispatch conn :player/set-active second-id false)
          ;; asker holds a :two, nobody target-relevant matters here --
          ;; force a miss so the turn actually advances, landing on
          ;; whichever seat is correctly next.
          (move-cards! conn asker-id [(first twos)])
          (force-deck-top-of-draw! conn (go-fish-deck (entity @conn minigame-id)) :three)
          (dispatch conn :go-fish/ask minigame-id asker-id third-id :two)
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 2)
              "index 1 is benched -- the stored index skips straight to
               index 2 instead of landing on a benched seat"))))))

;; --- Old Maid (example game) ---
(defn ^:private scene-old-maid [conn]
  (:camera/scene (:user/camera (user conn))))

(defn ^:private old-maid-sessions
  "Every Old Maid session currently on the scene -- unlike every other
   example game, more than one can legitimately exist at once (see
   events.cljs's 'Mini-game sessions' section), so tests exercising a
   single table resolve it via minigame-viewing-id below rather than
   assuming this returns exactly one."
  [conn]
  (filter (comp #{:old-maid} :minigame/kind) (:scene/minigames (scene-old-maid conn))))

(defn ^:private old-maid-session
  "The (assumed single) current Old Maid session -- only valid for
   tests that create exactly one; the multi-session tests resolve
   theirs via minigame-viewing-id instead."
  [conn]
  (first (old-maid-sessions conn)))

(defn ^:private old-maid-deck [minigame]
  (:minigame/deck minigame))

(defn ^:private old-maid-hand [minigame holder-id]
  (cards/cards-of-holder (:deck/cards (old-maid-deck minigame)) holder-id))

(defn ^:private old-maid-players
  "`minigame`'s participants, in seating order -- the session-scoped
   equivalent of the old, now-gone :scene/old-maid-players vector."
  [minigame]
  (mapv (comp :db/id :seat/player) (sort-by :seat/order (:minigame/seats minigame))))

(defn ^:private rank-with-copies
  "[rank cards] for some rank still holding >= n copies among
   `minigame`'s deck's remaining cards -- used by tests needing 2
   same-rank cards for a deterministic setup, since :old-maid/start's
   own auto-discard-at-deal-time means which ranks (if any) survive
   with all their copies intact varies from run to run (round-robin
   dealing a shuffled deck has real per-rank collision odds -- this is
   expected, not a bug, the same way a real physical deal can land two
   kings in the same hand by chance)."
  [minigame n]
  (let [by-rank (group-by :card/rank (:deck/cards (old-maid-deck minigame)))]
    (first (filter (fn [[_ cs]] (>= (count cs) n)) by-rank))))

(defn ^:private two-different-ranks-one-card-each
  "One card each from two DIFFERENT ranks still present in `minigame`'s
   deck -- used by tests needing a guaranteed non-match."
  [minigame]
  (map first (take 2 (vals (group-by :card/rank (:deck/cards (old-maid-deck minigame)))))))

(defn ^:private clear-old-maid-cards-to-draw!
  "Test helper: relocates EVERY card in `minigame`'s own deck back to
   the draw pile, holder cleared -- the Old-Maid-specific precursor to
   the generic clear-cards-to-draw! (defined near the top of this
   file, and used by every OTHER ported game's tests); kept as-is here
   rather than churned to match, since it's already correct."
  [conn minigame]
  (let [cards (:deck/cards (old-maid-deck minigame))]
    (transact! conn (mapcat (fn [c i] [{:db/id (:db/id c) :card/location :draw :card/position i}
                                        [:db/retract (:db/id c) :card/holder]])
                             cards (range)))))

(deftest test-old-maid-start
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:old-maid/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :old-maid/start ids)
      (let [minigame (old-maid-session conn)
            deck (old-maid-deck minigame)
            players (old-maid-players minigame)
            hands (map #(old-maid-hand minigame %) players)
            total-remaining (apply + (map count hands))]
        (is (= (set players) (set ids))
            "the turn cycle is exactly the participants given to
             :old-maid/start")
        (is (= (:minigame/turn-index minigame) 0))
        (is (= (:minigame/label minigame) "Old Maid 1"))
        (is (:minigame/neutral-authority? minigame))
        (is (not (:scene/neutral-authority? (scene-old-maid conn)))
            "the scene-wide flag is untouched -- Old Maid uses its own
             per-session flag instead")
        (is (empty? (:scene/decks (scene-old-maid conn)))
            "the session's deck is NOT also added to :scene/decks")
        (is (= (:deck/name deck) "Old Maid"))
        (is (= (count (:deck/cards deck)) total-remaining)
            "every surviving card is in exactly one hand -- the deck's own
             :deck/cards list and the sum of all hands always agree")
        (is (odd? total-remaining)
            "49 minus an even number of auto-discarded pairs is always odd,
             regardless of how many pairs the random deal happened to
             produce (round-robin dealing a shuffled deck has real
             per-rank collision odds -- discarding several pairs right
             at deal time is expected, not a bug)")
        (is (every? empty? (map old-maid/pairs-to-discard hands))
            "no hand holds a complete pair after start -- every pair the
             deal happened to produce was auto-discarded immediately")
        (is (= (count (filter (comp #{"Old Maid"} :card/label) (mapcat identity hands))) 1)
            "the single reskinned queen is always dealt to someone -- it
             never pairs, so auto-discard never touches it")))))

(deftest test-old-maid-start-rejected-without-element-enabled
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      ;; the active scene's game-type is still :default here --
      ;; :old-maid/game was never enabled.
      (dispatch conn :old-maid/start ids)
      (is (empty? (old-maid-sessions conn))
          "rejected -- :old-maid/start checks :old-maid/game is
           actually enabled on the scene's own game-type, not just
           relying on the UI to gate it, since any connected
           participant may dispatch this"))))

(deftest test-old-maid-draw-completes-pair
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:old-maid/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :old-maid/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-old-maid-cards-to-draw! conn (entity @conn minigame-id))
        (let [[drawer-id target-id] (old-maid-players (entity @conn minigame-id))
              [_ two-cards] (rank-with-copies (entity @conn minigame-id) 2)]
          (move-cards! conn drawer-id [(first two-cards)])
          (move-cards! conn target-id [(second two-cards)])
          (dispatch conn :old-maid/draw minigame-id drawer-id (:db/id (second two-cards)))
          (is (empty? (old-maid-hand (entity @conn minigame-id) drawer-id))
              "the drawn card completed a pair -- both vanish, none land in
               the drawer's hand at all")
          (is (empty? (old-maid-hand (entity @conn minigame-id) target-id)))
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 1)
              "the turn unconditionally advances to whoever was drawn from"))))))

(deftest test-old-maid-draw-no-match
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:old-maid/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :old-maid/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-old-maid-cards-to-draw! conn (entity @conn minigame-id))
        (let [[drawer-id target-id] (old-maid-players (entity @conn minigame-id))
              [a b] (two-different-ranks-one-card-each (entity @conn minigame-id))]
          (move-cards! conn drawer-id [a])
          (move-cards! conn target-id [b])
          (dispatch conn :old-maid/draw minigame-id drawer-id (:db/id b))
          (is (= (count (old-maid-hand (entity @conn minigame-id) drawer-id)) 2)
              "no match -- the drawn card just moves into the drawer's hand")
          (is (empty? (old-maid-hand (entity @conn minigame-id) target-id)))
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 1)))))))

(deftest test-old-maid-draw-by-non-participant-rejected
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:old-maid/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [[p1 p2 outsider] (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :old-maid/start [p1 p2])
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-old-maid-cards-to-draw! conn (entity @conn minigame-id))
        (let [[a b] (two-different-ranks-one-card-each (entity @conn minigame-id))]
          (move-cards! conn p1 [a])
          (move-cards! conn p2 [b])
          ;; `outsider` isn't seated at this session at all -- claiming
          ;; to BE the current turn player must be rejected the same
          ;; way a wrong card-id is: drawer-id has to equal the
          ;; RESOLVED current turn player, not just belong to someone.
          (dispatch conn :old-maid/draw minigame-id outsider (:db/id b))
          (is (= (count (old-maid-hand (entity @conn minigame-id) p1)) 1) "no-op -- p1 unaffected")
          (is (= (count (old-maid-hand (entity @conn minigame-id) p2)) 1) "no-op -- p2 unaffected")
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 0) "no-op, turn unchanged"))))))

(deftest test-old-maid-elimination-skips-empty-handed-player
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:old-maid/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :old-maid/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-old-maid-cards-to-draw! conn (entity @conn minigame-id))
        (let [[first-id _second-id third-id] (old-maid-players (entity @conn minigame-id))
              [a b] (two-different-ranks-one-card-each (entity @conn minigame-id))]
          ;; _second-id starts with an empty hand -- eliminated before the
          ;; game even really gets going, no separate event needed.
          (move-cards! conn first-id [a])
          (move-cards! conn third-id [b])
          (dispatch conn :old-maid/draw minigame-id first-id (:db/id b))
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 2)
              "index 1 (second-id) has no cards -- the stored index skips
               straight to index 2 instead of landing on an empty hand"))))))

(deftest test-old-maid-draw-after-external-bench-targets-drawers-real-neighbor
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:old-maid/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :old-maid/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-old-maid-cards-to-draw! conn (entity @conn minigame-id))
        (let [[first-id second-id third-id] (old-maid-players (entity @conn minigame-id))
              [a b] (two-different-ranks-one-card-each (entity @conn minigame-id))]
          ;; Bench the CURRENT turn holder (index 0) -- the stored
          ;; :minigame/turn-index (still 0) now points at a benched
          ;; seat, no longer matching second-id's own real position (1).
          ;; This reproduces exactly what a live manual-benching smoke
          ;; test caught: :old-maid/draw must derive "who's next" from the
          ;; DRAWER's own resolved position among the session's seats,
          ;; not from the raw stored index, or the drawer ends up
          ;; drawing from themselves (a spurious self-pair that
          ;; silently vanishes one of their own cards).
          (dispatch conn :player/set-active first-id false)
          (move-cards! conn second-id [a])
          (move-cards! conn third-id [b])
          (dispatch conn :old-maid/draw minigame-id second-id (:db/id b))
          (is (= (count (old-maid-hand (entity @conn minigame-id) second-id)) 2)
              "the drawer actually gained a card from someone else")
          (is (empty? (old-maid-hand (entity @conn minigame-id) third-id))
              "the card came from third-id, not a phantom duplicate of the
               drawer's own card")
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 2)
              "turn advances to third-id's real index, never back onto the
               drawer itself"))))))

(deftest test-old-maid-draw-wrong-card-id-rejected
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:old-maid/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :old-maid/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-old-maid-cards-to-draw! conn (entity @conn minigame-id))
        (let [[drawer-id target-id other-id] (old-maid-players (entity @conn minigame-id))
              [a b c] (take 3 (:deck/cards (old-maid-deck (entity @conn minigame-id))))]
          ;; target-id (the correctly-resolved neighbor) holds b; other-id
          ;; holds a DIFFERENT card, c. The drawer mistakenly tries to draw
          ;; c -- e.g. a stale UI click after hands changed underneath it.
          ;; :old-maid/draw must reject it: card-id must belong to the
          ;; RESOLVED neighbor's hand specifically, not just belong to
          ;; SOMEONE's hand.
          (move-cards! conn drawer-id [a])
          (move-cards! conn target-id [b])
          (move-cards! conn other-id [c])
          (dispatch conn :old-maid/draw minigame-id drawer-id (:db/id c))
          (is (= (count (old-maid-hand (entity @conn minigame-id) drawer-id)) 1) "no-op -- the drawer's hand is unchanged")
          (is (= (count (old-maid-hand (entity @conn minigame-id) other-id)) 1) "no-op -- other-id still holds their card")
          (is (= (count (old-maid-hand (entity @conn minigame-id) target-id)) 1) "target-id (the real neighbor) is untouched")
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 0) "no-op, turn unchanged"))))))

(deftest test-old-maid-seat-controller-override-beats-roster-controller
  (let [conn (ds/conn-from-db (initial-data false))
        roster-guest (random-uuid)
        seat-guest (random-uuid)]
    (transact! conn [{:db/id [:db/ident :user] :user/uuid seat-guest}])
    (add-conn! conn roster-guest)
    (add-conn! conn seat-guest)
    (set-enabled-elements! conn #{:old-maid/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [[p1 p2] (mapv :db/id (sort-by :db/id (root-players conn)))]
      ;; p1's ROSTER controller is roster-guest, but this ONE session
      ;; overrides p1's SEAT to seat-guest -- the local viewer here IS
      ;; seat-guest, so the per-session override must win.
      (dispatch conn :player/set-controller p1 [:user/uuid roster-guest])
      (dispatch conn :old-maid/start [p1 p2])
      (let [minigame-id (minigame-viewing-id conn)]
        (dispatch conn :minigame/set-controller minigame-id p1 seat-guest)
        (clear-old-maid-cards-to-draw! conn (entity @conn minigame-id))
        (let [[a b] (two-different-ranks-one-card-each (entity @conn minigame-id))]
          (move-cards! conn p1 [a])
          (move-cards! conn p2 [b])
          (dispatch conn :old-maid/draw minigame-id p1 (:db/id b))
          (is (= (count (old-maid-hand (entity @conn minigame-id) p1)) 2)
              "seat-guest -- not roster-guest -- is authorized to act
               for p1, since the per-session :seat/controller override
               takes priority over the roster's own :player/controller"))))))

(deftest test-old-maid-two-simultaneous-sessions-dont-interfere
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:old-maid/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [[p1 p2 p3 p4] (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :old-maid/start [p1 p2])
      (let [session-a-id (minigame-viewing-id conn)]
        (dispatch conn :old-maid/start [p3 p4])
        (let [session-b-id (minigame-viewing-id conn)]
          (is (not= session-a-id session-b-id))
          (is (= (count (old-maid-sessions conn)) 2))
          (is (= (set (map :minigame/label (old-maid-sessions conn))) #{"Old Maid 1" "Old Maid 2"})
              "sessions are auto-labeled distinctly")
          (is (= (set (old-maid-players (entity @conn session-a-id))) #{p1 p2}))
          (is (= (set (old-maid-players (entity @conn session-b-id))) #{p3 p4}))
          (let [deck-a-cards (set (map :db/id (:deck/cards (old-maid-deck (entity @conn session-a-id)))))
                deck-b-cards (set (map :db/id (:deck/cards (old-maid-deck (entity @conn session-b-id)))))]
            (is (empty? (set/intersection deck-a-cards deck-b-cards))
                "each session owns its own independent set of cards"))
          (clear-old-maid-cards-to-draw! conn (entity @conn session-a-id))
          (let [[drawer-id target-id] (old-maid-players (entity @conn session-a-id))
                [_ two-cards] (rank-with-copies (entity @conn session-a-id) 2)]
            (move-cards! conn drawer-id [(first two-cards)])
            (move-cards! conn target-id [(second two-cards)])
            (dispatch conn :old-maid/draw session-a-id drawer-id (:db/id (second two-cards)))
            (is (= (:minigame/turn-index (entity @conn session-a-id)) 1))
            (is (= (:minigame/turn-index (entity @conn session-b-id)) 0)
                "session B's turn index is completely unaffected by
                 session A's own draw")))))))

(deftest test-old-maid-same-player-in-two-sessions-has-separate-hands
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:old-maid/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [[p1 p2 p3] (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :old-maid/start [p1 p2])
      (let [session-a-id (minigame-viewing-id conn)]
        (dispatch conn :old-maid/start [p1 p3])
        (let [session-b-id (minigame-viewing-id conn)
              hand-a (set (map :db/id (old-maid-hand (entity @conn session-a-id) p1)))
              hand-b (set (map :db/id (old-maid-hand (entity @conn session-b-id) p1)))]
          (is (seq hand-a))
          (is (seq hand-b))
          (is (empty? (set/intersection hand-a hand-b))
              "p1's hand in session A and session B are disjoint card sets --
               the same roster player, two independent hands"))))))

(deftest test-old-maid-remove-cascades-without-touching-other-sessions
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:old-maid/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [[p1 p2 p3 p4] (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :old-maid/start [p1 p2])
      (let [session-a-id (minigame-viewing-id conn)]
        (dispatch conn :old-maid/start [p3 p4])
        (let [session-b-id (minigame-viewing-id conn)
              deck-a-id (:db/id (old-maid-deck (entity @conn session-a-id)))
              deck-b-id (:db/id (old-maid-deck (entity @conn session-b-id)))
              seat-a-ids (map :db/id (:minigame/seats (entity @conn session-a-id)))]
          (dispatch conn :minigame/remove session-a-id)
          (is (nil? (:db/id (entity @conn session-a-id))) "the session entity itself is gone")
          (is (nil? (:db/id (entity @conn deck-a-id))) "its deck is gone")
          (is (every? nil? (map (fn [id] (:db/id (entity @conn id))) seat-a-ids)) "its seats are gone")
          (is (some? (:db/id (entity @conn deck-b-id))) "the OTHER session's deck is untouched")
          (is (some? (:db/id (entity @conn session-b-id))) "the other session itself is untouched")
          (is (= (count (old-maid-sessions conn)) 1) "only the other session remains"))))))

(deftest test-old-maid-leaves-scene-neutral-authority-untouched
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:old-maid/game})
    (dispatch conn :scene/toggle-neutral-authority true)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :old-maid/start ids)
      (is (:scene/neutral-authority? (scene-old-maid conn))
          "starting a table doesn't touch the scene-wide flag -- it was
           already true (a manual DM setting) and stays true")
      (let [minigame-id (minigame-viewing-id conn)]
        (is (:minigame/neutral-authority? (entity @conn minigame-id))
            "the SESSION gets its own hand-visibility default instead")
        (dispatch conn :minigame/remove minigame-id)
        (is (:scene/neutral-authority? (scene-old-maid conn))
            "ending the table doesn't touch it either")))))

(deftest test-old-maid-remove
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:old-maid/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :old-maid/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            deck-id (:db/id (old-maid-deck (entity @conn minigame-id)))
            seat-ids (map :db/id (:minigame/seats (entity @conn minigame-id)))]
        (dispatch conn :minigame/remove minigame-id)
        (is (nil? (:db/id (entity @conn minigame-id))) "the session itself is retracted")
        (is (nil? (:db/id (entity @conn deck-id))) "the deck and its cards are retracted")
        (is (every? nil? (map (fn [id] (:db/id (entity @conn id))) seat-ids)) "its seats are retracted too")
        (is (empty? (old-maid-sessions conn)))))))

;; --- Crazy 8s (example game) ---
;; The fourth game ported onto the generic mini-game session
;; scaffolding (see events.cljs's 'Mini-game sessions' section) -- a
;; session-scoped prototype for letting several independent, arbitrary-
;; subset-of-the-roster tables run nested inside one scene at once.
(defn ^:private scene-crazy-eights [conn]
  (:camera/scene (:user/camera (user conn))))

(defn ^:private crazy-eights-sessions [conn]
  (filter (comp #{:crazy-eights} :minigame/kind) (:scene/minigames (scene-crazy-eights conn))))

(defn ^:private crazy-eights-session [conn]
  (first (crazy-eights-sessions conn)))

(defn ^:private crazy-eights-deck [minigame]
  (:minigame/deck minigame))

(defn ^:private crazy-eights-hand [minigame holder-id]
  (cards/cards-of-holder (:deck/cards (crazy-eights-deck minigame)) holder-id))

(defn ^:private crazy-eights-players [minigame]
  (mapv (comp :db/id :seat/player) (sort-by :seat/order (:minigame/seats minigame))))

(defn ^:private discard-top [minigame]
  (apply max-key :card/position (by-location (crazy-eights-deck minigame) :discard)))

(defn ^:private set-discard-top!
  "Test helper: makes `card` the sole live top-of-discard card of
   `minigame-id`'s deck and sets its :minigame/suit to `suit` --
   whatever was previously on top moves back into the draw pile (at
   positions guaranteed lower than `card`'s) so the discard pile never
   goes empty and `card` is unambiguously the new max-position (i.e.
   'top') card."
  [conn minigame-id card suit]
  (let [deck (crazy-eights-deck (entity @conn minigame-id))
        old-top (remove (comp #{(:db/id card)} :db/id) (by-location deck :discard))
        reclaim-tx (map-indexed
                    (fn [i c] {:db/id (:db/id c) :card/location :draw :card/position (- i)})
                    old-top)]
    (transact! conn
      (concat [{:db/id (:db/id card) :card/location :discard :card/position 0}
               [:db/retract (:db/id card) :card/holder]
               {:db/id minigame-id :minigame/suit suit}]
              reclaim-tx))))

(deftest test-crazy-eights-start
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:crazy-eights/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :crazy-eights/start ids)
      (let [minigame (crazy-eights-session conn)
            deck (crazy-eights-deck minigame)
            players (crazy-eights-players minigame)
            top (discard-top minigame)]
        (is (= (set players) (set ids))
            "the turn cycle is exactly the participants given to
             :crazy-eights/start")
        (is (= (:minigame/turn-index minigame) 0))
        (is (:minigame/neutral-authority? minigame))
        (is (nil? (:minigame/winner minigame)) "nobody's won yet")
        (is (empty? (:scene/decks (scene-crazy-eights conn)))
            "the session's deck is NOT also added to :scene/decks")
        (is (= (:deck/name deck) "Crazy 8s"))
        (is (= (count (:deck/cards deck)) 52) "the full deck, nothing removed")
        (is (= (count (by-location deck :hand)) 18) "6 cards dealt to each of 3 players")
        (is (every? #(= 6 (count (crazy-eights-hand minigame %))) players)
            "every participant gets EXACTLY 6 -- unlike Old Maid, Crazy 8s
             never auto-discards at deal time, so this is fully deterministic")
        (is (= (count (by-location deck :discard)) 1) "exactly one starting card is face up")
        (is (not= (:card/rank top) :eight)
            "the starter is never a wild 8 -- there'd be no declared suit yet")
        (is (= (:minigame/suit minigame) (:card/suit top))
            "the initial active suit is simply the starter's own printed suit")
        (is (= (count (by-location deck :draw)) (- 52 18 1))
            "everything not dealt or flipped stays in the draw pile")))))

(deftest test-crazy-eights-start-rejected-without-element-enabled
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :crazy-eights/start ids)
      (is (empty? (crazy-eights-sessions conn))
          "rejected -- :crazy-eights/start checks :crazy-eights/game is
           actually enabled on the scene's own game-type"))))

(deftest test-crazy-eights-play-legal-rank-match
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:crazy-eights/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :crazy-eights/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (crazy-eights-deck (entity @conn minigame-id)))
        (let [[player-id _next-id] (crazy-eights-players (entity @conn minigame-id))
              non-eights (remove (comp #{:eight} :card/rank) (:deck/cards (crazy-eights-deck (entity @conn minigame-id))))
              [_ [top-card hand-card]] (first (filter (fn [[_ cs]] (>= (count cs) 2))
                                                        (group-by :card/rank non-eights)))
              ;; A filler card so playing hand-card doesn't ALSO empty
              ;; the hand -- that would be a win, a different scenario
              ;; than "a normal legal play advances the turn".
              filler (first (remove (comp #{(:db/id top-card) (:db/id hand-card)} :db/id)
                                     (:deck/cards (crazy-eights-deck (entity @conn minigame-id)))))]
          (set-discard-top! conn minigame-id top-card (:card/suit top-card))
          (move-cards! conn player-id [hand-card filler])
          (dispatch conn :crazy-eights/play minigame-id player-id (:db/id hand-card) nil)
          (is (not (contains? (into #{} (map :db/id) (crazy-eights-hand (entity @conn minigame-id) player-id)) (:db/id hand-card)))
              "the played card leaves the player's hand")
          (is (= (:db/id (discard-top (entity @conn minigame-id))) (:db/id hand-card))
              "the played card becomes the new top of the discard pile")
          (is (= (:minigame/suit (entity @conn minigame-id)) (:card/suit hand-card))
              "a non-8 play's own suit becomes the new thing to match")
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 1)
              "the turn advances"))))))

(deftest test-crazy-eights-play-illegal-rejected
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:crazy-eights/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :crazy-eights/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (crazy-eights-deck (entity @conn minigame-id)))
        (let [[player-id _] (crazy-eights-players (entity @conn minigame-id))
              top-card (first (remove (comp #{:eight} :card/rank) (:deck/cards (crazy-eights-deck (entity @conn minigame-id)))))
              illegal-card (first (remove #(crazy-eights/playable? % top-card (:card/suit top-card))
                                           (:deck/cards (crazy-eights-deck (entity @conn minigame-id)))))]
          (set-discard-top! conn minigame-id top-card (:card/suit top-card))
          (move-cards! conn player-id [illegal-card])
          (dispatch conn :crazy-eights/play minigame-id player-id (:db/id illegal-card) nil)
          (is (contains? (into #{} (map :db/id) (crazy-eights-hand (entity @conn minigame-id) player-id)) (:db/id illegal-card))
              "the illegal card is rejected -- still in the player's hand")
          (is (= (:db/id (discard-top (entity @conn minigame-id))) (:db/id top-card)) "the discard top is unchanged")
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 0) "no-op, turn unchanged"))))))

(deftest test-crazy-eights-play-eight-declares-suit-and-constrains-next-player
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:crazy-eights/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :crazy-eights/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (crazy-eights-deck (entity @conn minigame-id)))
        (let [[player-a player-b] (crazy-eights-players (entity @conn minigame-id))
              top-card (first (remove (comp #{:eight} :card/rank) (:deck/cards (crazy-eights-deck (entity @conn minigame-id)))))
              eight (first (filter (comp #{:eight} :card/rank) (:deck/cards (crazy-eights-deck (entity @conn minigame-id)))))
              ;; A filler so playing the 8 doesn't ALSO empty player-a's
              ;; hand and win -- this test is about the declared suit,
              ;; not the win condition (see test-crazy-eights-win-on-
              ;; empty-hand).
              filler (first (remove (comp #{(:db/id top-card) (:db/id eight)} :db/id)
                                     (:deck/cards (crazy-eights-deck (entity @conn minigame-id)))))]
          (set-discard-top! conn minigame-id top-card (:card/suit top-card))
          (move-cards! conn player-a [eight filler])
          (dispatch conn :crazy-eights/play minigame-id player-a (:db/id eight) :hearts)
          (is (= (:db/id (discard-top (entity @conn minigame-id))) (:db/id eight)) "the wild 8 becomes the new top card")
          (is (= (:minigame/suit (entity @conn minigame-id)) :hearts)
              "the chosen suit is now what must be matched")
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 1) "turn advances to player-b")

          (testing "player-b, now facing a suit-less top card, can't play an off-suit non-8"
            (let [off-suit (first (remove #(crazy-eights/playable? % eight :hearts)
                                           (:deck/cards (crazy-eights-deck (entity @conn minigame-id)))))]
              (move-cards! conn player-b [off-suit])
              (dispatch conn :crazy-eights/play minigame-id player-b (:db/id off-suit) nil)
              (is (contains? (into #{} (map :db/id) (crazy-eights-hand (entity @conn minigame-id) player-b)) (:db/id off-suit))
                  "rejected -- still in hand")
              (is (= (:db/id (discard-top (entity @conn minigame-id))) (:db/id eight)) "discard top still unchanged")))

          (testing "but a card of the declared suit IS legal"
            (let [hearts-card (first (filter #(= (:card/suit %) :hearts) (:deck/cards (crazy-eights-deck (entity @conn minigame-id)))))]
              (move-cards! conn player-b [hearts-card])
              (dispatch conn :crazy-eights/play minigame-id player-b (:db/id hearts-card) nil)
              (is (= (:db/id (discard-top (entity @conn minigame-id))) (:db/id hearts-card))
                  "legal -- the declared suit constrained play, and this card satisfies it"))))))))

(deftest test-crazy-eights-play-by-non-participant-rejected
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:crazy-eights/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [[p1 p2 outsider] (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :crazy-eights/start [p1 p2])
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (crazy-eights-deck (entity @conn minigame-id)))
        (let [top-card (first (remove (comp #{:eight} :card/rank) (:deck/cards (crazy-eights-deck (entity @conn minigame-id)))))]
          (set-discard-top! conn minigame-id top-card (:card/suit top-card))
          (dispatch conn :crazy-eights/play minigame-id outsider (:db/id top-card) nil)
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 0) "no-op, turn unchanged"))))))

(deftest test-crazy-eights-draw-does-not-advance-turn
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:crazy-eights/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :crazy-eights/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (crazy-eights-deck (entity @conn minigame-id)))
        (let [[player-id _] (crazy-eights-players (entity @conn minigame-id))
              top-card (first (remove (comp #{:eight} :card/rank) (:deck/cards (crazy-eights-deck (entity @conn minigame-id)))))
              _ (set-discard-top! conn minigame-id top-card (:card/suit top-card))
              deck (crazy-eights-deck (entity @conn minigame-id))
              draw-before (by-location deck :draw)
              non-matching (take 6 (remove #(crazy-eights/playable? % top-card (:card/suit top-card)) draw-before))]
          (is (= (count non-matching) 6) "the draw pile has plenty of non-matching cards to build a stuck hand from")
          (move-cards! conn player-id non-matching)
          (let [draw-count-before (count (by-location (crazy-eights-deck (entity @conn minigame-id)) :draw))]
            (dispatch conn :crazy-eights/draw minigame-id player-id)
            (is (= (count (crazy-eights-hand (entity @conn minigame-id) player-id)) 7) "the player's hand grows by exactly 1")
            (is (= (count (by-location (crazy-eights-deck (entity @conn minigame-id)) :draw)) (dec draw-count-before))
                "exactly 1 card leaves the draw pile")
            (is (= (:minigame/turn-index (entity @conn minigame-id)) 0)
                "drawing never advances the turn -- the same player continues")))))))

(deftest test-crazy-eights-draw-reshuffles-discard-preserving-top-card
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:crazy-eights/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :crazy-eights/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (crazy-eights-deck (entity @conn minigame-id)))
        (let [[player-id _] (crazy-eights-players (entity @conn minigame-id))
              top-card (first (remove (comp #{:eight} :card/rank) (:deck/cards (crazy-eights-deck (entity @conn minigame-id)))))
              _ (set-discard-top! conn minigame-id top-card (:card/suit top-card))
              deck (crazy-eights-deck (entity @conn minigame-id))
              draw (by-location deck :draw)
              non-matching (take 6 (remove #(crazy-eights/playable? % top-card (:card/suit top-card)) draw))]
          (move-cards! conn player-id non-matching)
          ;; Empty the draw pile entirely -- every remaining :draw card
          ;; moves into the discard pile UNDER the live top card (lower
          ;; positions), simulating a long game where most of the deck
          ;; has been played.
          (let [deck (crazy-eights-deck (entity @conn minigame-id))
                remaining-draw (by-location deck :draw)
                pool-size (count remaining-draw)]
            (transact! conn
              (map-indexed (fn [i c] {:db/id (:db/id c) :card/location :discard :card/position (- (inc i))})
                            remaining-draw))
            (is (empty? (by-location (crazy-eights-deck (entity @conn minigame-id)) :draw)) "draw pile is now empty, by construction")
            (dispatch conn :crazy-eights/draw minigame-id player-id)
            (let [deck (crazy-eights-deck (entity @conn minigame-id))]
              (is (= (count (crazy-eights-hand (entity @conn minigame-id) player-id)) 7) "the player still gets exactly 1 new card")
              (is (= (count (by-location deck :draw)) (dec pool-size))
                  "the reclaimed discard pile (minus the 1 just drawn) is the new draw pile")
              (is (= (count (by-location deck :discard)) 1) "only the original top card remains in discard")
              (is (= (:db/id (discard-top (entity @conn minigame-id))) (:db/id top-card))
                  "the live top card itself was never touched by the reshuffle")
              (is (= (:minigame/turn-index (entity @conn minigame-id)) 0) "still no turn advance"))))))))

(deftest test-crazy-eights-win-on-empty-hand
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:crazy-eights/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :crazy-eights/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (crazy-eights-deck (entity @conn minigame-id)))
        (let [[player-id _] (crazy-eights-players (entity @conn minigame-id))
              top-card (first (remove (comp #{:eight} :card/rank) (:deck/cards (crazy-eights-deck (entity @conn minigame-id)))))
              ;; Excludes top-card's own id (every card trivially
              ;; "matches" itself by rank, so without this a naive scan
              ;; can pick top-card right back as "matching") AND
              ;; excludes 8s (always "playable" regardless of top/suit
              ;; -- an 8 landing here would need a real declared suit,
              ;; not the `nil` this test dispatches with).
              matching (some #(if (and (not= (:db/id %) (:db/id top-card))
                                        (not= (:card/rank %) :eight)
                                        (crazy-eights/playable? % top-card (:card/suit top-card)))
                                 %)
                             (:deck/cards (crazy-eights-deck (entity @conn minigame-id))))]
          (set-discard-top! conn minigame-id top-card (:card/suit top-card))
          (move-cards! conn player-id [matching])
          (dispatch conn :crazy-eights/play minigame-id player-id (:db/id matching) nil)
          (is (empty? (crazy-eights-hand (entity @conn minigame-id) player-id)) "the winning play empties the hand")
          (is (= (:minigame/winner (entity @conn minigame-id)) player-id))
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 0)
              "the turn index is left alone -- the game is over, not paused"))))))

(deftest test-crazy-eights-play-after-external-bench-targets-players-real-neighbor
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:crazy-eights/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :crazy-eights/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (crazy-eights-deck (entity @conn minigame-id)))
        (let [[first-id second-id _third-id] (crazy-eights-players (entity @conn minigame-id))
              top-card (first (remove (comp #{:eight} :card/rank) (:deck/cards (crazy-eights-deck (entity @conn minigame-id)))))
              ;; Excludes 8s too -- always "playable" regardless of
              ;; top/suit, but this test dispatches with a `nil` suit,
              ;; which would corrupt :minigame/suit if `matching`
              ;; happened to land on one.
              matching (some #(if (and (not= (:db/id %) (:db/id top-card))
                                        (not= (:card/rank %) :eight)
                                        (crazy-eights/playable? % top-card (:card/suit top-card)))
                                 %)
                             (:deck/cards (crazy-eights-deck (entity @conn minigame-id))))
              ;; A filler so playing `matching` doesn't ALSO empty
              ;; second-id's hand and win -- this test is about turn
              ;; resolution, not the win condition.
              filler (first (remove (comp #{(:db/id top-card) (:db/id matching)} :db/id)
                                     (:deck/cards (crazy-eights-deck (entity @conn minigame-id)))))]
          ;; Bench the CURRENT turn holder (index 0) -- the stored
          ;; :minigame/turn-index (still 0) now points at a benched
          ;; seat, no longer matching second-id's own real position (1).
          ;; Same regression Old Maid's live smoke test first caught:
          ;; :crazy-eights/play must derive 'next' from the PLAYER's own
          ;; resolved position, not the raw stored index, or the turn
          ;; would land right back on second-id instead of advancing to
          ;; third-id.
          (set-discard-top! conn minigame-id top-card (:card/suit top-card))
          (dispatch conn :player/set-active first-id false)
          (move-cards! conn second-id [matching filler])
          (dispatch conn :crazy-eights/play minigame-id second-id (:db/id matching) nil)
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 2)
              "turn advances to third-id's real index, never back onto second-id itself"))))))

(deftest test-crazy-eights-remove
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:crazy-eights/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :crazy-eights/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            deck-id (:db/id (crazy-eights-deck (entity @conn minigame-id)))
            seat-ids (map :db/id (:minigame/seats (entity @conn minigame-id)))]
        (dispatch conn :minigame/remove minigame-id)
        (is (nil? (:db/id (entity @conn minigame-id))) "the session itself is retracted")
        (is (nil? (:db/id (entity @conn deck-id))) "the deck and its cards are retracted")
        (is (every? nil? (map (fn [id] (:db/id (entity @conn id))) seat-ids)) "its seats are retracted too")
        (is (empty? (crazy-eights-sessions conn)))))))

(deftest test-crazy-eights-leaves-scene-neutral-authority-untouched
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:crazy-eights/game})
    (dispatch conn :scene/toggle-neutral-authority true)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :crazy-eights/start ids)
      (is (:scene/neutral-authority? (scene-crazy-eights conn))
          "starting a table doesn't touch the scene-wide flag")
      (let [minigame-id (minigame-viewing-id conn)]
        (is (:minigame/neutral-authority? (entity @conn minigame-id))
            "the SESSION gets its own hand-visibility default instead")
        (dispatch conn :minigame/remove minigame-id)
        (is (:scene/neutral-authority? (scene-crazy-eights conn))
            "ending the table doesn't touch it either")))))

;; --- Rummy (example game) ---
;; The fifth game ported onto the generic mini-game session scaffolding
;; (see events.cljs's 'Mini-game sessions' section) -- a session-scoped
;; prototype for letting several independent, arbitrary-subset-of-the-
;; roster tables run nested inside one scene at once.
(defn ^:private scene-rummy [conn]
  (:camera/scene (:user/camera (user conn))))

(defn ^:private rummy-sessions [conn]
  (filter (comp #{:rummy} :minigame/kind) (:scene/minigames (scene-rummy conn))))

(defn ^:private rummy-session [conn]
  (first (rummy-sessions conn)))

(defn ^:private rummy-deck [minigame]
  (:minigame/deck minigame))

(defn ^:private rummy-hand [minigame holder-id]
  (cards/cards-of-holder (:deck/cards (rummy-deck minigame)) holder-id))

(defn ^:private rummy-scored [minigame]
  (by-location (rummy-deck minigame) :scored))

(defn ^:private rummy-players [minigame]
  (mapv (comp :db/id :seat/player) (sort-by :seat/order (:minigame/seats minigame))))

(deftest test-rummy-start
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:rummy/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :rummy/start ids)
      (let [minigame (rummy-session conn)
            deck (rummy-deck minigame)
            players (rummy-players minigame)]
        (is (= (set players) (set ids))
            "the turn cycle is exactly the participants given to
             :rummy/start")
        (is (= (:minigame/turn-index minigame) 0))
        (is (false? (:minigame/drawn? minigame)))
        (is (:minigame/neutral-authority? minigame))
        (is (empty? (:scene/decks (scene-rummy conn)))
            "the session's deck is NOT also added to :scene/decks")
        (is (= (:deck/name deck) "Standard 52-Card Deck")
            "the purest reuse case yet -- no deck modification at all")
        (is (= (count (:deck/cards deck)) 52))
        (is (every? #(= 6 (count (rummy-hand minigame %))) players)
            "every participant gets EXACTLY 6 -- no auto-discard at deal
             time the way Old Maid has, fully deterministic")
        (is (= (count (by-location deck :discard)) 1) "exactly one starting card is face up")
        (is (= (count (by-location deck :draw)) (- 52 18 1))
            "everything not dealt or flipped stays in the draw pile")))))

(deftest test-rummy-start-rejected-without-element-enabled
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :rummy/start ids)
      (is (empty? (rummy-sessions conn))
          "rejected -- :rummy/start checks :rummy/game is actually
           enabled on the scene's own game-type"))))

(deftest test-rummy-draw-from-pile
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:rummy/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :rummy/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            [player-id _] (rummy-players (entity @conn minigame-id))
            draw-before (count (by-location (rummy-deck (entity @conn minigame-id)) :draw))
            hand-before (count (rummy-hand (entity @conn minigame-id) player-id))]
        (dispatch conn :rummy/draw-from-pile minigame-id player-id)
        (is (= (count (rummy-hand (entity @conn minigame-id) player-id)) (inc hand-before)))
        (is (= (count (by-location (rummy-deck (entity @conn minigame-id)) :draw)) (dec draw-before)))
        (is (:minigame/drawn? (entity @conn minigame-id)))))))

(deftest test-rummy-draw-from-discard
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:rummy/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :rummy/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            [player-id _] (rummy-players (entity @conn minigame-id))
            top (apply max-key :card/position (by-location (rummy-deck (entity @conn minigame-id)) :discard))
            hand-before (count (rummy-hand (entity @conn minigame-id) player-id))]
        (dispatch conn :rummy/draw-from-discard minigame-id player-id)
        (is (contains? (into #{} (map :db/id) (rummy-hand (entity @conn minigame-id) player-id)) (:db/id top)))
        (is (= (count (rummy-hand (entity @conn minigame-id) player-id)) (inc hand-before)))
        (is (empty? (by-location (rummy-deck (entity @conn minigame-id)) :discard))
            "momentarily empty -- refilled by this player's own mandatory discard, same turn")
        (is (:minigame/drawn? (entity @conn minigame-id)))))))

(deftest test-rummy-second-draw-same-turn-rejected
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:rummy/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :rummy/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            [player-id _] (rummy-players (entity @conn minigame-id))]
        (dispatch conn :rummy/draw-from-pile minigame-id player-id)
        (let [hand-after-first (count (rummy-hand (entity @conn minigame-id) player-id))
              draw-after-first (count (by-location (rummy-deck (entity @conn minigame-id)) :draw))]
          (dispatch conn :rummy/draw-from-pile minigame-id player-id)
          (is (= (count (rummy-hand (entity @conn minigame-id) player-id)) hand-after-first) "no-op, already drawn this turn")
          (is (= (count (by-location (rummy-deck (entity @conn minigame-id)) :draw)) draw-after-first))
          (dispatch conn :rummy/draw-from-discard minigame-id player-id)
          (is (= (count (rummy-hand (entity @conn minigame-id) player-id)) hand-after-first)
              "no-op -- the OTHER draw action is equally blocked, one draw total per turn"))))))

(deftest test-rummy-discard-before-draw-rejected
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:rummy/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :rummy/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            [player-id _] (rummy-players (entity @conn minigame-id))
            card (first (rummy-hand (entity @conn minigame-id) player-id))]
        (dispatch conn :rummy/discard minigame-id player-id (:db/id card))
        (is (contains? (into #{} (map :db/id) (rummy-hand (entity @conn minigame-id) player-id)) (:db/id card))
            "no-op -- must draw before discarding")
        (is (= (:minigame/turn-index (entity @conn minigame-id)) 0))))))

(deftest test-rummy-discard-ends-turn
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:rummy/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :rummy/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            [player-id _] (rummy-players (entity @conn minigame-id))]
        (dispatch conn :rummy/draw-from-pile minigame-id player-id)
        (let [card (first (rummy-hand (entity @conn minigame-id) player-id))]
          (dispatch conn :rummy/discard minigame-id player-id (:db/id card))
          (is (not (contains? (into #{} (map :db/id) (rummy-hand (entity @conn minigame-id) player-id)) (:db/id card))))
          (is (= (:db/id (apply max-key :card/position (by-location (rummy-deck (entity @conn minigame-id)) :discard)))
                 (:db/id card)))
          (is (false? (:minigame/drawn? (entity @conn minigame-id)))
              "cleared -- the next player must draw before they can discard too")
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 1)))))))

(deftest test-rummy-score-fresh-set
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:rummy/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :rummy/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (rummy-deck (entity @conn minigame-id)))
        (let [[player-id other-id] (rummy-players (entity @conn minigame-id))
              sevens (filter (comp #{:seven} :card/rank) (:deck/cards (rummy-deck (entity @conn minigame-id))))
              ;; A filler for the OTHER seated player -- clear-cards-to-
              ;; draw! emptied every hand, and rummy-finished? treats
              ;; ANY seated player's empty hand as game-over, which
              ;; would reject this score outright before it's even
              ;; about the fresh-set logic being tested here.
              filler (first (remove (comp #{:seven} :card/rank) (:deck/cards (rummy-deck (entity @conn minigame-id)))))]
          (move-cards! conn player-id (take 3 sevens))
          (move-cards! conn other-id [filler])
          (dispatch conn :rummy/score minigame-id player-id :seven)
          (is (= (count (rummy-scored (entity @conn minigame-id))) 3))
          (is (every? #(= (:db/id (:card/holder %)) player-id) (rummy-scored (entity @conn minigame-id)))))))))

(deftest test-rummy-score-lay-off-fourth-by-different-player
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:rummy/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :rummy/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (rummy-deck (entity @conn minigame-id)))
        (let [[player-a player-b] (rummy-players (entity @conn minigame-id))
              sevens (filter (comp #{:seven} :card/rank) (:deck/cards (rummy-deck (entity @conn minigame-id))))
              ;; A filler for player-a so scoring their 3 sevens doesn't
              ;; ALSO empty their hand and end the game (rummy-finished?
              ;; would then reject player-b's later lay-off outright) --
              ;; this test is about shared-credit lay-off, not game-end.
              filler (first (remove (comp #{:seven} :card/rank) (:deck/cards (rummy-deck (entity @conn minigame-id)))))]
          (move-cards! conn player-a (conj (vec (take 3 sevens)) filler))
          (move-cards! conn player-b [(nth sevens 3)])
          (dispatch conn :rummy/score minigame-id player-a :seven)
          (dispatch conn :rummy/score minigame-id player-b :seven)
          (is (= (count (rummy-scored (entity @conn minigame-id))) 4) "all 4 sevens now scored")
          (let [scores (frequencies (map (comp :db/id :card/holder) (rummy-scored (entity @conn minigame-id))))]
            (is (= (get scores player-a) 3))
            (is (= (get scores player-b) 1)
                "player-b gets individual credit for laying off the 4th, even
                 though player-a started the set")))))))

(deftest test-rummy-score-over-full-rank-rejected
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:rummy/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :rummy/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (rummy-deck (entity @conn minigame-id)))
        (let [[player-id other-id] (rummy-players (entity @conn minigame-id))
              sevens (filter (comp #{:seven} :card/rank) (:deck/cards (rummy-deck (entity @conn minigame-id))))
              ;; A filler for the OTHER seated player -- see the same
              ;; note in test-rummy-score-fresh-set.
              filler (first (remove (comp #{:seven} :card/rank) (:deck/cards (rummy-deck (entity @conn minigame-id)))))]
          (move-cards! conn player-id sevens)
          (move-cards! conn other-id [filler])
          (dispatch conn :rummy/score minigame-id player-id :seven)
          (is (= (count (rummy-scored (entity @conn minigame-id))) 4))
          (dispatch conn :rummy/score minigame-id player-id :seven)
          (is (= (count (rummy-scored (entity @conn minigame-id))) 4) "no-op -- the rank is already fully scored"))))))

(deftest test-rummy-score-run-requires-element-enabled
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:rummy/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :rummy/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (rummy-deck (entity @conn minigame-id)))
        (let [[player-id other-id] (rummy-players (entity @conn minigame-id))
              run-cards (filter (fn [c] (and (= (:card/suit c) :hearts)
                                              (contains? #{:five :six :seven} (:card/rank c))))
                                 (:deck/cards (rummy-deck (entity @conn minigame-id))))
              ;; A filler for the OTHER seated player -- see the same
              ;; note in test-rummy-score-fresh-set.
              filler (first (remove (comp (set (map :db/id run-cards)) :db/id)
                                     (:deck/cards (rummy-deck (entity @conn minigame-id)))))]
          (move-cards! conn player-id run-cards)
          (move-cards! conn other-id [filler])
          (dispatch conn :rummy/score-run minigame-id player-id (mapv :db/id run-cards))
          (is (empty? (rummy-scored (entity @conn minigame-id))) "no-op -- :rummy/runs isn't enabled by default"))))))

(deftest test-rummy-score-run-when-enabled
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:rummy/game :rummy/runs})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :rummy/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (rummy-deck (entity @conn minigame-id)))
        (let [[player-id other-id] (rummy-players (entity @conn minigame-id))
              run-cards (filter (fn [c] (and (= (:card/suit c) :hearts)
                                              (contains? #{:five :six :seven} (:card/rank c))))
                                 (:deck/cards (rummy-deck (entity @conn minigame-id))))
              ;; A filler for the OTHER seated player -- see the same
              ;; note in test-rummy-score-fresh-set.
              filler (first (remove (comp (set (map :db/id run-cards)) :db/id)
                                     (:deck/cards (rummy-deck (entity @conn minigame-id)))))]
          (move-cards! conn player-id run-cards)
          (move-cards! conn other-id [filler])
          (dispatch conn :rummy/score-run minigame-id player-id (mapv :db/id run-cards))
          (is (= (count (rummy-scored (entity @conn minigame-id))) 3))
          (is (every? #(= (:db/id (:card/holder %)) player-id) (rummy-scored (entity @conn minigame-id)))))))))

(deftest test-rummy-draw-from-pile-reshuffles-discard-preserving-top-card
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:rummy/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :rummy/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            [player-id _] (rummy-players (entity @conn minigame-id))
            deck (rummy-deck (entity @conn minigame-id))
            top-card (apply max-key :card/position (by-location deck :discard))
            remaining-draw (by-location deck :draw)
            pool-size (count remaining-draw)]
        ;; Empty the draw pile entirely -- every remaining :draw card
        ;; moves into the discard pile UNDER the live top card (lower
        ;; positions), simulating a long game where most of the deck has
        ;; been played.
        (transact! conn
          (map-indexed (fn [i c] {:db/id (:db/id c) :card/location :discard :card/position (- (inc i))})
                        remaining-draw))
        (is (empty? (by-location (rummy-deck (entity @conn minigame-id)) :draw)) "draw pile is now empty, by construction")
        (dispatch conn :rummy/draw-from-pile minigame-id player-id)
        (let [deck (rummy-deck (entity @conn minigame-id))]
          (is (= (count (by-location deck :draw)) (dec pool-size))
              "the reclaimed discard pile (minus the 1 just drawn) is the new draw pile")
          (is (= (count (by-location deck :discard)) 1) "only the original top card remains in discard")
          (is (= (:db/id (apply max-key :card/position (by-location deck :discard))) (:db/id top-card))
              "the live top card itself was never touched by the reshuffle")
          (is (:minigame/drawn? (entity @conn minigame-id))))))))

(deftest test-rummy-game-ends-and-tally-can-differ-from-who-emptied
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:rummy/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :rummy/start ids)
      (let [minigame-id (minigame-viewing-id conn)]
        (clear-cards-to-draw! conn (rummy-deck (entity @conn minigame-id)))
        (let [[player-a player-b] (rummy-players (entity @conn minigame-id))
              kings (filter (comp #{:king} :card/rank) (:deck/cards (rummy-deck (entity @conn minigame-id))))
              sevens (filter (comp #{:seven} :card/rank) (:deck/cards (rummy-deck (entity @conn minigame-id))))
              filler (take 2 (remove (comp #{:king :seven} :card/rank) (:deck/cards (rummy-deck (entity @conn minigame-id)))))]
          ;; player-a's ENTIRE hand is exactly 3 kings -- scoring them
          ;; empties it completely and ends the game, without ever
          ;; needing a turn or a discard (scoring is never turn-gated).
          (move-cards! conn player-a (take 3 kings))
          ;; player-b holds all 4 sevens plus 2 unrelated cards -- their
          ;; hand stays non-empty even after scoring the sevens.
          (move-cards! conn player-b (concat sevens filler))
          ;; player-b scores FIRST, while the game is still active --
          ;; player-a's own score (below) is what actually ends it, and
          ;; once it does, rummy-finished? correctly blocks anything
          ;; further, so ordering matters: player-b's score must land
          ;; before player-a's does.
          (dispatch conn :rummy/score minigame-id player-b :seven)
          (dispatch conn :rummy/score minigame-id player-a :king)
          (is (empty? (rummy-hand (entity @conn minigame-id) player-a)) "player-a's hand is empty -- the game has ended")
          (is (seq (rummy-hand (entity @conn minigame-id) player-b)) "player-b's hand is NOT empty -- they didn't end the game")
          (let [scores (frequencies (map (comp :db/id :card/holder) (rummy-scored (entity @conn minigame-id))))]
            (is (= (get scores player-a) 3))
            (is (= (get scores player-b) 4))
            (is (> (get scores player-b) (get scores player-a))
                "player-b holds MORE scored cards despite NOT being the one
                 who ended the game -- the tallied winner (turn-order/
                 winners over this same scores map, computed live by
                 panel_rummy.cljs) would correctly be player-b, not
                 player-a")))))))

(deftest test-rummy-discard-after-external-bench-targets-players-real-neighbor
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:rummy/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :rummy/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            [first-id second-id _third-id] (rummy-players (entity @conn minigame-id))]
        ;; Bench the CURRENT turn holder (index 0) -- the stored
        ;; :minigame/turn-index (still 0) now points at a benched seat,
        ;; no longer matching second-id's own real position (1). Same
        ;; regression class Old Maid's live smoke test first caught,
        ;; and Crazy 8s' own analogous test guards against too.
        (dispatch conn :player/set-active first-id false)
        (dispatch conn :rummy/draw-from-pile minigame-id second-id)
        (let [card (first (rummy-hand (entity @conn minigame-id) second-id))]
          (dispatch conn :rummy/discard minigame-id second-id (:db/id card))
          (is (= (:minigame/turn-index (entity @conn minigame-id)) 2)
              "turn advances to third-id's real index, never back onto second-id itself"))))))

(deftest test-rummy-remove
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:rummy/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :rummy/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            deck-id (:db/id (rummy-deck (entity @conn minigame-id)))
            seat-ids (map :db/id (:minigame/seats (entity @conn minigame-id)))]
        (dispatch conn :minigame/remove minigame-id)
        (is (nil? (:db/id (entity @conn minigame-id))) "the session itself is retracted")
        (is (nil? (:db/id (entity @conn deck-id))) "the deck and its cards are retracted")
        (is (every? nil? (map (fn [id] (:db/id (entity @conn id))) seat-ids)) "its seats are retracted too")
        (is (empty? (rummy-sessions conn)))))))

(deftest test-rummy-leaves-scene-neutral-authority-untouched
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:rummy/game})
    (dispatch conn :scene/toggle-neutral-authority true)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :rummy/start ids)
      (is (:scene/neutral-authority? (scene-rummy conn))
          "starting a table doesn't touch the scene-wide flag")
      (let [minigame-id (minigame-viewing-id conn)]
        (is (:minigame/neutral-authority? (entity @conn minigame-id))
            "the SESSION gets its own hand-visibility default instead")
        (dispatch conn :minigame/remove minigame-id)
        (is (:scene/neutral-authority? (scene-rummy conn))
            "ending the table doesn't touch it either")))))

;; --- War (example game) ---
;; The sixth and last game ported onto the generic mini-game session
;; scaffolding (see events.cljs's 'Mini-game sessions' section) -- a
;; session-scoped prototype for letting several independent, arbitrary-
;; subset-of-the-roster tables run nested inside one scene at once.
(defn ^:private scene-war [conn]
  (:camera/scene (:user/camera (user conn))))

(defn ^:private war-sessions [conn]
  (filter (comp #{:war} :minigame/kind) (:scene/minigames (scene-war conn))))

(defn ^:private war-session [conn]
  (first (war-sessions conn)))

(defn ^:private war-deck [minigame]
  (:minigame/deck minigame))

(defn ^:private war-players [minigame]
  (mapv (comp :db/id :seat/player) (sort-by :seat/order (:minigame/seats minigame))))

(defn ^:private war-pile [minigame player-id location]
  (filter (fn [c] (and (= (:card/location c) location) (= (:db/id (:card/holder c)) player-id)))
          (:deck/cards (war-deck minigame))))

(defn ^:private war-total [minigame player-id]
  (+ (count (war-pile minigame player-id :draw)) (count (war-pile minigame player-id :won))))

(defn ^:private set-war-draw!
  "Test helper: makes `cards` (first = top) EXACTLY `player-id`'s
   :draw pile, with no :won cards at all -- any of their OTHER current
   cards (e.g. left over from :war/start's natural deal, in EITHER
   pile) are shunted to `sink-id`'s :won pile instead, out of the way,
   keeping the 'every card belongs to someone' invariant intact while
   leaving `player-id`'s own piles an exact, uncontaminated match for
   `cards` -- callers bench `sink-id` (or fully strip-player! it
   afterward) so it doesn't show up as a live contender itself."
  [conn minigame player-id sink-id cards]
  (let [wanted-ids (into #{} (map :db/id) cards)
        leftover (concat (remove (comp wanted-ids :db/id) (war-pile minigame player-id :draw))
                          (war-pile minigame player-id :won))
        sink-start (count (war-pile minigame sink-id :won))]
    (transact! conn
      (concat
       (map-indexed (fn [i c] {:db/id (:db/id c) :card/location :won
                                :card/holder sink-id :card/position (+ sink-start i)})
                     leftover)
       (map-indexed (fn [i c] {:db/id (:db/id c) :card/location :draw
                                :card/holder player-id :card/position (- (count cards) 1 i)})
                     cards)))))

(defn ^:private strip-player!
  "Test helper: moves ALL of `player-id`'s cards (both piles) to
   `to-id`'s :won pile, leaving `player-id` with nothing at all --
   used to construct a deterministic 'already eliminated' scenario."
  [conn minigame player-id to-id]
  (let [cards (concat (war-pile minigame player-id :draw) (war-pile minigame player-id :won))
        start (count (war-pile minigame to-id :won))]
    (transact! conn
      (map-indexed (fn [i c] {:db/id (:db/id c) :card/location :won
                               :card/holder to-id :card/position (+ start i)})
                    cards))))

(deftest test-war-start
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:war/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :war/start ids)
      (let [minigame (war-session conn)
            deck (war-deck minigame)
            players (war-players minigame)]
        (is (= (set players) (set ids))
            "the players are exactly the participants given to
             :war/start")
        (is (nil? (:minigame/contenders minigame)))
        (is (nil? (:minigame/last-round minigame)))
        (is (empty? (:scene/decks (scene-war conn)))
            "the session's deck is NOT also added to :scene/decks")
        (is (= (count (:deck/cards deck)) 52) "the full deck, nothing removed")
        (is (every? #(= (:card/location %) :draw) (:deck/cards deck))
            "the ENTIRE deck goes straight into personal draw piles -- no shared pile at all")
        (is (every? :card/holder (:deck/cards deck))
            "every single card belongs to exactly one player from the start")
        (is (= (apply + (map #(war-total minigame %) players)) 52))
        (is (= (set (map #(war-total minigame %) players)) #{17 18})
            "round-robin dealing 52 cards among 3 players -- 18/17/17")))))

(deftest test-war-start-rejected-without-element-enabled
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :war/start ids)
      (is (empty? (war-sessions conn))
          "rejected -- :war/start checks :war/game is actually enabled
           on the scene's own game-type"))))

(deftest test-war-round-clear-winner
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:war/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :war/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            [p1 p2 sink] (war-players (entity @conn minigame-id))
            king (first (filter (comp #{:king} :card/rank) (:deck/cards (war-deck (entity @conn minigame-id)))))
            two (first (filter (comp #{:two} :card/rank) (:deck/cards (war-deck (entity @conn minigame-id)))))]
        (dispatch conn :player/set-active sink false)
        (set-war-draw! conn (entity @conn minigame-id) p1 sink [king])
        (set-war-draw! conn (entity @conn minigame-id) p2 sink [two])
        (dispatch conn :war/play-round minigame-id)
        (is (nil? (:minigame/contenders (entity @conn minigame-id))) "resolved -- no ongoing war")
        (is (= (:winner-id (:minigame/last-round (entity @conn minigame-id))) p1))
        (is (= (:cards-won (:minigame/last-round (entity @conn minigame-id))) 2))
        (is (= (set (map :db/id (war-pile (entity @conn minigame-id) p1 :won))) #{(:db/id king) (:db/id two)})
            "p1 wins both cards played this round")
        (is (empty? (war-pile (entity @conn minigame-id) p2 :won)))
        (is (empty? (filter (comp #{:war} :card/location) (:deck/cards (war-deck (entity @conn minigame-id)))))
            "the pot is empty again -- fully swept to the winner")))))

(deftest test-war-tie-then-continue
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:war/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :war/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            [p1 p2 sink] (war-players (entity @conn minigame-id))
            sevens (filter (comp #{:seven} :card/rank) (:deck/cards (war-deck (entity @conn minigame-id))))
            [s1 s2] (take 2 sevens)
            king (first (filter (comp #{:king} :card/rank) (:deck/cards (war-deck (entity @conn minigame-id)))))
            three (first (filter (comp #{:three} :card/rank) (:deck/cards (war-deck (entity @conn minigame-id)))))]
        (dispatch conn :player/set-active sink false)
        (set-war-draw! conn (entity @conn minigame-id) p1 sink [s1 king])
        (set-war-draw! conn (entity @conn minigame-id) p2 sink [s2 three])
        (dispatch conn :war/play-round minigame-id)
        (is (= (set (:minigame/contenders (entity @conn minigame-id))) #{p1 p2}) "both played a 7 -- tied, war!")
        (is (nil? (:minigame/last-round (entity @conn minigame-id))) "not resolved yet")
        (is (= (count (filter (comp #{:war} :card/location) (:deck/cards (war-deck (entity @conn minigame-id))))) 2)
            "both 7s sit in the pot")
        (is (empty? (war-pile (entity @conn minigame-id) p1 :won)))
        (dispatch conn :war/play-round minigame-id)
        (is (nil? (:minigame/contenders (entity @conn minigame-id))) "resolved -- p1's king beats p2's three")
        (is (= (:winner-id (:minigame/last-round (entity @conn minigame-id))) p1))
        (is (= (:cards-won (:minigame/last-round (entity @conn minigame-id))) 4))
        (is (= (set (map :db/id (war-pile (entity @conn minigame-id) p1 :won)))
               #{(:db/id s1) (:db/id s2) (:db/id king) (:db/id three)})
            "the WHOLE accumulated pot -- both 7s plus both escalation cards -- goes to p1")
        (is (empty? (filter (comp #{:war} :card/location) (:deck/cards (war-deck (entity @conn minigame-id))))))))))

(deftest test-war-personal-reshuffle
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:war/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :war/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            [p1 p2 sink] (war-players (entity @conn minigame-id))
            two (first (filter (comp #{:two} :card/rank) (:deck/cards (war-deck (entity @conn minigame-id)))))]
        (dispatch conn :player/set-active sink false)
        (set-war-draw! conn (entity @conn minigame-id) p1 sink [])
        (set-war-draw! conn (entity @conn minigame-id) p2 sink [two])
        ;; Give p1 a concrete, non-empty :won pile (relocated from sink,
        ;; which already absorbed p1's original leftovers) -- :draw
        ;; stays genuinely empty, forcing a reshuffle on their next
        ;; draw.
        (let [p1-won (take 3 (war-pile (entity @conn minigame-id) sink :won))]
          (transact! conn
            (map-indexed (fn [i c] {:db/id (:db/id c) :card/location :won :card/holder p1 :card/position i})
                         p1-won)))
        (let [p1-total-before (war-total (entity @conn minigame-id) p1)
              p2-total-before (war-total (entity @conn minigame-id) p2)]
          (is (empty? (war-pile (entity @conn minigame-id) p1 :draw)) "p1's draw pile is genuinely empty")
          (is (pos? (count (war-pile (entity @conn minigame-id) p1 :won))) "everything they hold sits in :won, unshuffled")
          (dispatch conn :war/play-round minigame-id)
          (is (= (+ (war-total (entity @conn minigame-id) p1) (war-total (entity @conn minigame-id) p2))
                 (+ p1-total-before p2-total-before))
              "no card lost or duplicated by the reshuffle -- the combined
               total between the two participants is conserved, regardless
               of who actually wins the round")
          (is (empty? (filter (comp #{:war} :card/location) (:deck/cards (war-deck (entity @conn minigame-id)))))
              "the round still resolved -- reshuffling didn't block play"))))))

(deftest test-war-elimination-drops-empty-handed-player
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:war/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :war/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            [p1 p2 p3] (war-players (entity @conn minigame-id))
            king (first (filter (comp #{:king} :card/rank) (:deck/cards (war-deck (entity @conn minigame-id)))))
            two (first (filter (comp #{:two} :card/rank) (:deck/cards (war-deck (entity @conn minigame-id)))))]
        (set-war-draw! conn (entity @conn minigame-id) p1 p3 [king])
        (set-war-draw! conn (entity @conn minigame-id) p2 p3 [two])
        ;; p3 has NOTHING at all in either pile -- eliminated before
        ;; this round even starts, no separate event needed.
        (strip-player! conn (entity @conn minigame-id) p3 p1)
        (dispatch conn :war/play-round minigame-id)
        (is (= (:winner-id (:minigame/last-round (entity @conn minigame-id))) p1))
        (is (= (:cards-won (:minigame/last-round (entity @conn minigame-id))) 2)
            "only p1's and p2's cards -- p3 never participated at all")))))

(deftest test-war-finished-when-one-player-has-everything
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:war/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :war/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            [p1 p2] (war-players (entity @conn minigame-id))]
        (strip-player! conn (entity @conn minigame-id) p2 p1)
        (is (zero? (war-total (entity @conn minigame-id) p2)) "p2 holds nothing -- eliminated")
        (is (= (war-total (entity @conn minigame-id) p1) 52) "p1 holds the entire deck -- the win condition")))))

(deftest test-war-remove
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:war/game})
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :war/start ids)
      (let [minigame-id (minigame-viewing-id conn)
            deck-id (:db/id (war-deck (entity @conn minigame-id)))
            seat-ids (map :db/id (:minigame/seats (entity @conn minigame-id)))]
        (dispatch conn :minigame/remove minigame-id)
        (is (nil? (:db/id (entity @conn minigame-id))) "the session itself is retracted")
        (is (nil? (:db/id (entity @conn deck-id))) "the deck and its cards are retracted")
        (is (every? nil? (map (fn [id] (:db/id (entity @conn id))) seat-ids)) "its seats are retracted too")
        (is (empty? (war-sessions conn)))))))

(deftest test-war-never-touches-scene-neutral-authority
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:war/game})
    (dispatch conn :scene/toggle-neutral-authority true)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :war/start ids)
      (is (:scene/neutral-authority? (scene-war conn))
          "War has no privacy concept at all -- it never touches the
           scene-wide flag, unlike every other example game")
      (let [minigame-id (minigame-viewing-id conn)]
        (dispatch conn :minigame/remove minigame-id)
        (is (:scene/neutral-authority? (scene-war conn)))))))

;; --- Neutral authority (impartial dealer) mode ---
(deftest test-scene-toggle-neutral-authority
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :scene/toggle-neutral-authority true)
    (is (:scene/neutral-authority? (scene-memory conn)))
    (dispatch conn :scene/toggle-neutral-authority false)
    (is (false? (:scene/neutral-authority? (scene-memory conn))))))

(deftest test-objects-toggle-hidden-host-rejected-in-neutral-scene
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :token/create (Vec2. 0 0) nil)
    (dispatch conn :scene/toggle-neutral-authority true)
    (let [id (:db/id (scene-token conn))]
      (dispatch conn :objects/toggle-hidden id)
      (is (not (:object/hidden (entity @conn id)))
          "on a neutral-authority scene, the host gets no automatic
           default authority either -- an unowned/unassigned object is
           authorized for no one until explicitly owned/controlled or
           :object/shared?"))))

(deftest test-objects-toggle-hidden-host-allowed-once-neutral-authority-off
  (let [conn (ds/conn-from-db (initial-data true))]
    (dispatch conn :token/create (Vec2. 0 0) nil)
    (dispatch conn :scene/toggle-neutral-authority true)
    (dispatch conn :scene/toggle-neutral-authority false)
    (let [id (:db/id (scene-token conn))]
      (dispatch conn :objects/toggle-hidden id)
      (is (:object/hidden (entity @conn id))
          "reverting to the default (non-neutral) scene restores the
           host's ordinary fallback authority"))))

(deftest test-memory-leaves-scene-neutral-authority-untouched
  (let [conn (ds/conn-from-db (initial-data true))]
    (set-enabled-elements! conn #{:memory/game})
    (dispatch conn :scene/toggle-neutral-authority true)
    (dispatch conn :player/create :human)
    (dispatch conn :player/create :human)
    (let [ids (mapv :db/id (sort-by :db/id (root-players conn)))]
      (dispatch conn :memory/start ids)
      (is (:scene/neutral-authority? (scene-memory conn))
          "starting a table doesn't touch the scene-wide flag -- it was
           already true (a manual DM setting) and stays true")
      (let [minigame-id (minigame-viewing-id conn)]
        (is (:minigame/neutral-authority? (entity @conn minigame-id))
            "the SESSION gets its own hand-visibility default instead --
             Memory needs impartiality by default, the host is often
             also a competing player")
        (dispatch conn :minigame/remove minigame-id)
        (is (:scene/neutral-authority? (scene-memory conn))
            "ending the table doesn't touch it either")))))
