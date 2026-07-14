(ns ogres.app.modifiers
  (:require [ogres.app.geom :as geom]
            [ogres.app.vec :as vec :refer [Vec2]]))

(defn trunc [params]
  (let [dx (.. params -transform -x)
        dy (.. params -transform -y)]
    (js/Object.assign
     #js {} (.-transform params)
     #js {"x" (js/Math.trunc dx)
          "y" (js/Math.trunc dy)})))

(defn scale-fn [scale grid-type]
  (fn [params]
    (let [dx (.. params -transform -x)
          dy (.. params -transform -y)
          v  (geom/screen->scene-vec (Vec2. dx dy) scale grid-type)]
      (js/Object.assign
       #js {} (.-transform params)
       #js {"x" (.-x v)
            "y" (.-y v)}))))
