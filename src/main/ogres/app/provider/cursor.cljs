(ns ogres.app.provider.cursor
  (:require [ogres.app.geom :as geom]
            [ogres.app.hooks :as hooks]
            [ogres.app.segment :as seg]
            [ogres.app.vec :as vec :refer [Vec2]]
            [uix.core :as uix :refer [defui]]))

(def ^:private handler-query
  [[:user/bounds :default seg/zero]
   {:user/camera
    [[:camera/scale :default 1]
     [:camera/point :default vec/zero]
     {:camera/scene [[:scene/grid-type :default :square]]}]}])

(defui listeners []
  (let [publish (hooks/use-publish)
        result  (hooks/use-query handler-query)
        {bounds :user/bounds
         {point :camera/point
          scale :camera/scale
          {grid-type :scene/grid-type} :camera/scene} :user/camera} result]
    (hooks/use-event-listener js/window "pointermove"
      (uix/use-callback
       (fn [event]
         (if-let [element (.. event -target (closest "#scene-drag"))]
           (let [data (.-dataset element)]
             (if (and (some? data) (= (.-dragging data) "false"))
               (let [dx (.-clientX event)
                     dy (.-clientY event)
                     mv (vec/add (geom/screen->scene-vec (vec/sub (Vec2. dx dy) (.-a bounds)) scale grid-type) point)]
                 (publish :cursor/move (.-x mv) (.-y mv)))))))
       [publish point bounds scale grid-type]))))
