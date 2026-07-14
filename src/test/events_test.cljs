(ns events-test
  (:require [cljs.test :refer-macros [deftest is]]
            [datascript.core :as ds :refer [transact! entity]]
            [ogres.app.const :refer [grid-size half-size hex-radius]]
            [ogres.app.events :refer [event-tx-fn]]
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
