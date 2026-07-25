(ns events-test
  (:require [cljs.test :refer-macros [deftest is]]
            [datascript.core :as ds :refer [transact! entity]]
            [ogres.app.const :refer [grid-size half-size hex-radius]]
            [ogres.app.events :refer [event-tx-fn]]
            [ogres.app.game-type :as game-type]
            [ogres.app.geom :as geom]
            [ogres.app.provider.state :refer [initial-data]]
            [ogres.app.vec :as vec :refer [Vec2]]))

(defn dispatch [conn event & args]
  (transact! conn [[:db.fn/call (fn [db] (apply event-tx-fn db event args))]]))

(defn user [conn]
  (entity @conn [:db/ident :user]))

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
        scene (:camera/scene (:user/camera (user conn)))]
    (is (= (count game-types) 3)
        "The bundled 'Default', 'D&D 5e', and 'Gloomhaven' game-types are
         all seeded on a fresh db.")
    (is (= (:game-type/name default) "Default"))
    (is (= (:game-type/name dnd5e) "D&D 5e"))
    (is (= (:game-type/name gloomhaven) "Gloomhaven"))
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
      (is (= (count (:root/game-types (root conn))) 4)
          "The new game-type is linked into :root/game-types alongside the
           three bundled templates (Default, D&D 5e, Gloomhaven)."))))

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
    ;; nothing to exclude when turning things off.
    (dispatch conn :game-type/toggle-category game-type-id
              (conj gloomhaven-ids :tool/grid-hex-pointy) false)
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

