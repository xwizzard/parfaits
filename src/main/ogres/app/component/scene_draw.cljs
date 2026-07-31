(ns ogres.app.component.scene-draw
  (:require [clojure.string :refer [join]]
            [ogres.app.component :refer [icon]]
            [ogres.app.const :refer [grid-size half-size hex-radius]]
            [ogres.app.geom :as geom]
            [ogres.app.hooks :as hooks]
            [ogres.app.matrix :as matrix]
            [ogres.app.util :as util]
            [ogres.app.segment :as seg :refer [Segment]]
            [ogres.app.vec :as vec :refer [Vec2]]
            [uix.core :as uix :refer [defui $]]
            ["@dnd-kit/core"
             :refer  [DndContext useDraggable useDndMonitor]
             :rename {DndContext    dnd-context
                      useDndMonitor use-dnd-monitor
                      useDraggable  use-draggable}]))

(defn instance [x]
  (cond (instance? Vec2 x) Vec2
        (instance? Segment x) Segment))

;; Every align-fn below takes the point/segment being aligned plus the
;; scene's current :scene/grid-type -- draw-segment passes this through
;; uniformly to whichever align-fn a draw tool selected (see its own
;; `align` calls), even though only align-cell-center actually uses it.
;; A shared 2-arg dispatch fn (dispatching on the first arg only) keeps
;; every align-fn callable the same way regardless of arity needs.
(defn ^:private align-dispatch [x _] (instance x))

(defmulti align-identity align-dispatch)
(defmethod align-identity Vec2 [a _] a)
(defmethod align-identity Segment [s _] s)

(defmulti align-grid align-dispatch)
(defmethod align-grid Vec2 [a _]
  (vec/rnd a grid-size))
(defmethod align-grid Segment [s grid-type]
  (Segment. (align-grid (.-a s) grid-type) (align-grid (.-b s) grid-type)))

(defmulti align-grid-half align-dispatch)
(defmethod align-grid-half Vec2 [a _] (vec/rnd a half-size))
(defmethod align-grid-half Segment [s grid-type]
  (Segment. (align-grid-half (.-a s) grid-type) (align-grid-half (.-b s) grid-type)))

(defmulti align-line align-dispatch)
(defmethod align-line Vec2 [a grid-type] (align-grid-half a grid-type))
(defmethod align-line Segment [s grid-type]
  (let [src (align-grid-half (.-a s) grid-type)
        dir (vec/sub (.-b s) src)
        len (vec/dist vec/zero dir)]
    (if (= len 0)
      (Segment. src src)
      (let [dst (-> (vec/div dir len) (vec/mul (util/round len grid-size)) (vec/add src))]
        (Segment. src dst)))))

(defmulti align-cone align-dispatch)
(defmethod align-cone Vec2 [a grid-type] (align-grid a grid-type))
(defmethod align-cone Segment [s grid-type]
  (let [src (align-grid (.-a s) grid-type)
        dir (vec/sub (.-b s) src)
        len (vec/dist vec/zero dir)]
    (if (= len 0)
      (Segment. src src)
      (let [dst (-> (vec/div dir len) (vec/mul (util/round len grid-size)) (vec/add src))]
        (Segment. src dst)))))

(defn ^:private nearest-cell-center
  "The center of whichever grid cell (hex or square, dispatched the same
   way ogres.app.geom/cell-distance and snap-to-cell already do) is
   nearest the given point."
  [point grid-type]
  (case (geom/base-grid-type grid-type)
    :hex-pointy (vec/nearest-hex point hex-radius)
    :hex-flat (vec/nearest-hex-flat point hex-radius)
    (vec/nearest-square point grid-size)))

(defmulti align-cell-center
  "Snaps to the nearest whole grid-cell CENTER (never a corner or
   edge midpoint), hex- or square-aware -- used by the ruler so
   measurement endpoints always land on a cell center, unlike
   align-grid-half's plain half-cell rounding (still used by
   align-line for shape drawing)."
  align-dispatch)
(defmethod align-cell-center Vec2 [a grid-type]
  (nearest-cell-center a grid-type))
(defmethod align-cell-center Segment [s grid-type]
  (Segment. (align-cell-center (.-a s) grid-type) (align-cell-center (.-b s) grid-type)))

(def ^:private points->poly
  (completing into (fn [xs] (join " " xs))))

(defn ^:private px->distance
  "Scene pixels -> the NUMBER shown on a measurement label, in whatever
   unit the active game-type measures in.

   Nothing about the scene's own geometry changes here: the grid is
   always `grid-size` px per cell and a token's :token/size, light and
   aura stay in fixed scene units. This converts px to cells and then
   asks how much of the game's own unit one cell represents -- 5 feet is
   D&D's convention and the default, but a cell is just as legitimately
   6 feet, 2 metres, a hex, or a parsec. Only the presentation moves."
  [len per-cell]
  (let [n  (* (/ len grid-size) per-cell)
        rd (js/Math.round n)]
    (if (< (abs (- n rd)) 0.001) rd
        (.toFixed n 1))))

(defn ^:private distance-label
  "px->distance plus the game-type's unit suffix, e.g. \"15ft.\" or
   \"4.5m\". The suffix is concatenated directly, so it carries its own
   leading space if it wants one."
  [len {:keys [per-cell unit]}]
  (str (px->distance len per-cell) unit))

;; A small dedicated query, independent of draw-segment's shared `query`
;; above (and its children-fn signature every other draw tool relies on)
;; -- mirrors scene_context_menu.cljs's `use-enabled-elements` pattern.
;; Only draw-ruler uses this; the other draw tools are untouched.
(def ^:private measurement-query
  [{:user/camera
    [{:camera/scene
      [[:scene/grid-type :default :square]
       {:scene/game-type
        [[:game-type/enabled-elements :default #{}]
         [:game-type/distance-per-cell :default 5]
         [:game-type/distance-unit :default "ft."]]}]}]}])

(defn ^:private use-measurement-info []
  (let [result (hooks/use-query measurement-query)
        scene  (-> result :user/camera :camera/scene)]
    {:grid-type (:scene/grid-type scene)
     :enabled   (:game-type/enabled-elements (:scene/game-type scene) #{})
     :per-cell  (:game-type/distance-per-cell (:scene/game-type scene) 5)
     :unit      (:game-type/distance-unit (:scene/game-type scene) "ft.")}))

(defn ^:private format-measurement
  "Builds the ruler's distance label from whichever measurement
   primitive(s) are enabled -- :tool/measurement (a real-world distance
   in the game-type's own unit, see distance-label) and/or
   :tool/measurement-cells (grid-cell count, hex- or square-aware, see
   ogres.app.geom/cell-distance). The two aren't
   exclusive: if both are enabled, both show, joined by \" / \". If
   neither is (only reachable today via the pre-existing gap where the
   'r' keyboard shortcut doesn't check :tool/measurement -- see
   toolbar.cljs), this is an empty string."
  [segment {:keys [grid-type enabled] :as info}]
  (join " / "
        (cond-> []
          (contains? enabled :tool/measurement)
          (conj (distance-label (vec/dist-cheb segment) info))
          (contains? enabled :tool/measurement-cells)
          (conj (let [n (geom/cell-distance segment grid-type)]
                  (str n (if (= n 1) " cell" " cells")))))))

(defui ^:private text [props]
  ($ :text.scene-text.scene-text-draw
    (dissoc props :children)
    (:children props)))

(defui ^:private anchor [props]
  ($ :g {:transform (:transform props)}
    ($ :circle.scene-draw-anchor {:r 4})
    ($ :circle.scene-draw-anchor-ring {:r 6})))

(defui ^:private draw-segment-drag [props]
  (let [{:keys [children on-release use-cursor]} props
        [segment set-segment] (uix/use-state nil)
        [cursor   set-cursor] (uix/use-state nil)
        options (use-draggable #js {"id" "drawable"})
        on-down (.. options -listeners -onPointerDown)
        on-stop
        (fn []
          (when (not (nil? segment))
            (set-segment nil)
            (set-cursor nil)
            (on-release segment)))
        on-drag
        (uix/use-callback
         (fn [data]
           (let [dx (.-x (.-delta data))
                 dy (.-y (.-delta data))
                 mx (.-clientX (.-activatorEvent data))
                 my (.-clientY (.-activatorEvent data))]
             (set-segment
              (fn [s]
                (if (some? s)
                  (Segment. (.-a s) (vec/add (.-a s) (Vec2. dx dy)))
                  (Segment. (Vec2. mx my) (Vec2. mx my))))))) [])
        on-move
        (uix/use-callback
         (fn [event]
           (set-cursor (Vec2. (.-clientX event) (.-clientY event)))) [])]
    (use-dnd-monitor
     #js {"onDragMove" on-drag
          "onDragEnd"  on-stop})
    ($ :<>
      ($ :rect.scene-draw-surface
        {:ref (.-setNodeRef options)
         :on-pointer-down on-down
         :on-pointer-move (if (and use-cursor (nil? segment)) on-move)})
      (children segment cursor))))

(def ^:private query
  [[:user/bounds :default seg/zero]
   {:user/camera
    [[:camera/scale :default 1]
     [:camera/point :default vec/zero]
     {:camera/scene
      [[:scene/grid-align :default false]
       [:scene/grid-origin :default vec/zero]
       [:scene/grid-size :default grid-size]
       [:scene/grid-type :default :square]
       [:scene/show-object-outlines :default true]]}]}])

(defui ^:private draw-segment [props]
  (let [{:keys [children on-release tile-path align-fn]
         :or {on-release :default align-fn align-identity}} props
        result (hooks/use-query query)
        {bounds :user/bounds
         {point :camera/point
          scale :camera/scale
          {grid-paths :scene/show-object-outlines
           grid-align :scene/grid-align
           grid-type :scene/grid-type}
          :camera/scene}
         :user/camera} result
        align (if grid-align align-fn align-identity)
        basis  (matrix/multiply (matrix/translate matrix/identity point) (geom/scene-scale-matrix scale grid-type))
        camera (matrix/translate basis (vec/mul (.-a bounds) -1))
        invert (matrix/inverse basis)]
    ($ draw-segment-drag
      {:use-cursor (contains? props :align-fn)
       :on-release
       (fn [segment]
         (on-release (align (camera segment) grid-type)))}
      (fn [segment cursor]
        (cond (some? segment)
              (let [segment (align (camera segment) grid-type)]
                ($ :<>
                  (if (and (fn? tile-path) grid-paths)
                    (let [path (tile-path segment)]
                      ($ :polygon.scene-draw-tile-path
                        {:points (transduce (map (comp seq invert)) points->poly [] path)})))
                  (children segment (invert segment))))
              (and grid-align (some? cursor))
              ($ anchor {:transform (invert (align (camera cursor) grid-type))}))))))

(defui ^:private polygon
  [{:keys [on-create]}]
  (let [result (hooks/use-query query)
        {bounds :user/bounds
         {point :camera/point
          scale :camera/scale
          {align? :scene/grid-align
           grid-type :scene/grid-type} :camera/scene} :user/camera} result
        [points set-points] (uix/use-state [])
        [cursor set-cursor] (uix/use-state nil)
        closing? (and (seq points) (some? cursor) (< (vec/dist (first points) cursor) 32))
        basis  (matrix/multiply (matrix/translate matrix/identity point) (geom/scene-scale-matrix scale grid-type))
        camera (matrix/translate basis (vec/mul (.-a bounds) -1))
        invert (matrix/inverse basis)]
    ($ :<>
      ($ :rect.scene-draw-surface
        {:on-pointer-down
         (fn [event]
           (.stopPropagation event))
         :on-pointer-move
         (fn [event]
           (let [point (Vec2. (.-clientX event) (.-clientY event))]
             (set-cursor (vec/rnd (camera point) (if align? half-size 1)))))
         :on-click
         (fn [event]
           (if (not closing?)
             (set-points (conj points cursor))
             (let [xs (geom/reorient (mapcat seq points))
                   xf (comp (partition-all 2) (map (fn [[x y]] (Vec2. x y))))]
               (set-points [])
               (set-cursor nil)
               (on-create event (into [] xf xs)))))})
      (if (and align? (not closing?) (some? cursor))
        ($ anchor {:transform (invert cursor)}))
      (if (seq points)
        ($ :circle.scene-draw-point-ring
          {:transform (invert (first points)) :r 6}))
      (for [point points :let [point (invert point)]]
        ($ :circle.scene-draw-point
          {:key point :transform point :r 4}))
      (if (and (seq points) (some? cursor))
        ($ :polygon.scene-draw-shape
          {:points
           (transduce
            (map (comp seq invert))
            points->poly []
            (if (not closing?) (conj points cursor) points))})))))

(defui ^:private draw-select []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:on-release (fn [s] (dispatch :selection/from-rect s))}
      (fn [_ canvas]
        (let [a (.-a canvas) b (.-b canvas)]
          ($ hooks/use-portal {:name :multiselect}
            ($ :path.scene-draw-shape
              {:d (join " " [\M (.-x a) (.-y a) \H (.-x b) \V (.-y b) \H (.-x a) \Z])})))))))

(defui ^:private draw-ruler []
  (let [info (use-measurement-info)]
    ($ draw-segment
      {:align-fn align-cell-center}
      (fn [camera canvas]
        (let [a (.-a canvas) b (.-b canvas)]
          ($ :<>
            ($ :line.scene-draw-shape
              {:x1 (.-x a)
               :y1 (.-y a)
               :x2 (.-x b)
               :y2 (.-y b)})
            ($ anchor {:transform a})
            ($ anchor {:transform b})
            (let [point (seg/extend canvas 32)]
              ($ text {:x (.-x point) :y (.-y point)}
                (format-measurement camera info)))))))))

(defui ^:private draw-circle []
  (let [dispatch (hooks/use-dispatch)
        info (use-measurement-info)]
    ($ draw-segment
      {:align-fn align-grid
       :on-release (fn [s] (dispatch :shape/create :circle (seq s)))
       :tile-path
       (fn [s]
         (let [r (vec/dist-cheb s)]
           (geom/tile-path-circle (.-a s) r)))}
      (fn [camera canvas]
        (let [src (.-a canvas)]
          ($ :<>
            ($ :circle.scene-draw-shape
              {:transform src :r (vec/dist-cheb canvas)})
            ($ text {:x (.-x src) :y (.-y src)}
              (str (distance-label (vec/dist-cheb camera) info) " radius"))))))))

(defui ^:private draw-rect []
  (let [dispatch (hooks/use-dispatch)
        info (use-measurement-info)]
    ($ draw-segment
      {:align-fn align-grid
       :on-release (fn [s] (dispatch :shape/create :rect (seq s)))}
      (fn [camera canvas]
        (let [a (.-a camera) b (.-b camera)
              c (.-a canvas) d (.-b canvas)]
          ($ :<>
            ($ :path.scene-draw-shape
              {:d (join " " [\M (.-x c) (.-y c) \H (.-x d) \V (.-y d) \H (.-x c) \Z])})
            (let [point (seg/extend canvas 32)]
              ($ text {:x (.-x point) :y (.-y point)}
                (let [v (vec/abs (vec/sub a b))]
                  (str (distance-label (.-x v) info) " x "
                       (distance-label (.-y v) info)))))))))))

(defui ^:private draw-line []
  (let [dispatch (hooks/use-dispatch)
        info (use-measurement-info)]
    ($ draw-segment
      {:align-fn align-line
       :on-release (fn [s] (dispatch :shape/create :line (seq s)))
       :tile-path
       (fn [s]
         (geom/tile-path-line (geom/line-points s)))}
      (fn [camera canvas]
        (let [scale (/ (vec/dist canvas) (vec/dist camera))]
          ($ :<>
            (let [points (geom/line-points canvas (* half-size scale))]
              ($ :polygon.scene-draw-shape
                {:points (join " " (mapcat seq points))}))
            (let [point (seg/extend canvas 32)]
              ($ text {:x (.-x point) :y (.-y point)}
                (distance-label (vec/dist camera) info)))))))))

(defui ^:private draw-cone []
  (let [dispatch (hooks/use-dispatch)
        info (use-measurement-info)]
    ($ draw-segment
      {:align-fn align-cone
       :on-release (fn [s] (dispatch :shape/create :cone (seq s)))
       :tile-path
       (fn [s]
         (geom/tile-path-cone (geom/cone-points s)))}
      (fn [camera canvas]
        ($ :<>
          (let [points (geom/cone-points canvas)]
            ($ :polygon.scene-draw-shape
              {:points (join " " (mapcat seq points))}))
          (let [point (seg/extend canvas 32)]
            ($ text {:x (.-x point) :y (.-y point)}
              (distance-label (vec/dist camera) info))))))))

(defui ^:private draw-poly []
  (let [dispatch (hooks/use-dispatch)]
    ($ polygon
      {:on-create
       (fn [_ points]
         (dispatch :shape/create :poly points))})))

(defui ^:private draw-mask []
  (let [dispatch (hooks/use-dispatch)]
    ($ polygon
      {:on-create
       (fn [_ points]
         (dispatch :mask/create (mapcat seq points)))})))

(defui ^:private draw-grid []
  (let [dispatch (hooks/use-dispatch)
        {bounds :user/bounds
         {shift :camera/point
          scale   :camera/scale
          {prev-size :scene/grid-size
           prev-origin :scene/grid-origin
           grid-type :scene/grid-type}
          :camera/scene} :user/camera} (hooks/use-query query)
        [origin set-origin] (uix/use-state nil)
        [size     set-size] (uix/use-state prev-size)
        basis (.-a bounds)
        on-shift (fn [a] (fn [] (set-origin (fn [b] (vec/add a b)))))]
    ($ :g.grid-align
      ($ :rect.scene-draw-surface
        {:on-click
         (fn [event]
           (set-origin (Vec2. (.-clientX event) (.-clientY event))))})
      (if (some? origin)
        (let [rows 7
              draw (* size scale (/ grid-size prev-size))
              wide (* draw (inc rows))
              path (for [step (range (- rows) (inc rows))]
                     (str "M " (* step draw) " " (- wide)      " " \V " " wide " "
                          "M " (- wide)      " " (* step draw) " " \H " " wide " "))]
          ($ :g {:transform (vec/sub origin basis)}
            ($ :path.grid-align-path {:d (join path)})
            ($ :circle.grid-align-center {:r 6})
            ($ :foreignObject.grid-align-form
              {:x -128 :y -128 :width 256 :height 256}
              ($ :form
                {:on-submit
                 (fn [event]
                   (.preventDefault event)
                   (dispatch
                    :scene/apply-grid-options
                    (-> (vec/sub origin basis)
                        (geom/screen->scene-vec scale grid-type)
                        (vec/add shift)
                        (vec/add (or prev-origin vec/zero))
                        (vec/mul (/ prev-size size))
                        (vec/abs)
                        (vec/mod grid-size)
                        (vec/rnd 0.25)) size))}
                ($ :fieldset.grid-align-origin
                  ($ :button
                    {:type "button" :data-name "up" :on-click (on-shift (Vec2. 0 -1))}
                    ($ icon {:name "arrow-up-short" :size 20}))
                  ($ :button
                    {:type "button" :data-name "right" :on-click (on-shift (Vec2. 1 0))}
                    ($ icon {:name "arrow-right-short" :size 20}))
                  ($ :button
                    {:type "button" :data-name "down" :on-click (on-shift (Vec2. 0 1))}
                    ($ icon {:name "arrow-down-short" :size 20}))
                  ($ :button
                    {:type "button" :data-name "left" :on-click (on-shift (Vec2. -1 0))}
                    ($ icon {:name "arrow-left-short" :size 20}))
                  (if (not= prev-origin vec/zero)
                    ($ :button
                      {:type "button" :data-name "clear" :data-tooltip "Reset"
                       :on-click (fn [] (dispatch :scene/reset-grid-origin))}
                      ($ icon {:name "x-circle-fill" :size 16}))))
                ($ :fieldset.grid-align-size
                  ($ :button
                    {:type "button" :data-name "dec" :on-click (fn [] (set-size dec))}
                    ($ icon {:name "dash" :size 20}))
                  ($ :button
                    {:type "button" :data-name "inc" :on-click (fn [] (set-size inc))}
                    ($ icon {:name "plus" :size 20}))
                  ($ :input.text.text-ghost
                    {:type "number"
                     :value size
                     :style {:color "white"}
                     :on-change
                     (fn [event]
                       (let [n (js/Number (.. event -target -value))]
                         (if (number? n)
                           (set-size n))))})
                  ($ :button {:type "submit" :data-name "submit"}
                    ($ icon {:name "check"})))))))))))

(defui ^:private draw-note []
  (let [dispatch (hooks/use-dispatch)]
    ($ :rect.scene-draw-surface
      {:on-click
       (fn [event]
         (let [point (Vec2. (.-clientX event) (.-clientY event))]
           (dispatch :note/create point)))})))

(defui draw [{:keys [mode] :as props}]
  ($ dnd-context
    (case mode
      :circle ($ draw-circle props)
      :cone   ($ draw-cone props)
      :grid   ($ draw-grid props)
      :line   ($ draw-line props)
      :mask   ($ draw-mask props)
      :note   ($ draw-note props)
      :poly   ($ draw-poly props)
      :rect   ($ draw-rect props)
      :ruler  ($ draw-ruler props)
      :select ($ draw-select props))))
