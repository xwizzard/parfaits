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
        scene (:camera/scene (:user/camera (user conn)))]
    (is (= (count game-types) 1)
        "Exactly the bundled 'Default' game-type is seeded on a fresh db.")
    (is (= (:game-type/name default) "Default"))
    (is (contains? (:game-type/enabled-elements default) :unit/light)
        "The seeded default enables every real (non-reserved) element.")
    (is (not (contains? (:game-type/enabled-elements default) :system/hp-tracker))
        "The reserved hp-tracker element is not enabled by default.")
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
      (is (= (count (:root/game-types (root conn))) 2)
          "The new game-type is linked into :root/game-types alongside the
           bundled default."))))

(deftest test-game-type-toggle-element
  (let [conn (ds/conn-from-db (initial-data true))
        game-type-id (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn)))))]
    (dispatch conn :game-type/toggle-element game-type-id :unit/light false)
    (is (not (contains? (:game-type/enabled-elements (entity @conn game-type-id)) :unit/light))
        "Toggling an element off removes it from the enabled set.")
    (dispatch conn :game-type/toggle-element game-type-id :unit/light true)
    (is (contains? (:game-type/enabled-elements (entity @conn game-type-id)) :unit/light)
        "Toggling an element back on restores it.")))

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
    (dispatch conn :game-type/set-icon-override game-type-id :unit/conditions {:icon/url "https://example.com/icon.svg"})
    (is (= (get (:game-type/icon-overrides (entity @conn game-type-id)) :unit/conditions)
           {:icon/url "https://example.com/icon.svg"})
        "Setting an icon override stores the link under the element id.")
    (dispatch conn :game-type/set-icon-override game-type-id :unit/conditions nil)
    (is (not (contains? (:game-type/icon-overrides (entity @conn game-type-id)) :unit/conditions))
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
        default-id (:db/id (:scene/game-type (:camera/scene (:user/camera (user conn)))))]
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
