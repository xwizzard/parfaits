(ns ogres.app.component.scene-objects
  (:require [clojure.string :refer [join]]
            [ogres.app.component :refer [icon]]
            [ogres.app.component.card-pile :as card-pile]
            [ogres.app.component.scene-context-menu :refer [context-menu]]
            [ogres.app.component.scene-pattern :refer [pattern]]
            [ogres.app.const :refer [grid-size hex-radius]]
            [ogres.app.game-type :as game-type]
            [ogres.app.geom :as geom]
            [ogres.app.hooks :as hooks]
            [ogres.app.matrix :as matrix]
            [ogres.app.modifiers :as modifiers]
            [ogres.app.memory :as memory]
            [ogres.app.player :as player]
            [ogres.app.segment :as seg :refer [Segment]]
            [ogres.app.util :as util]
            [ogres.app.vec :as vec :refer [Vec2]]
            [react-transition-group :refer [TransitionGroup CSSTransition]]
            [uix.core :as uix :refer [defui $]]
            [uix.dom :as dom]
            ["@dnd-kit/core"
             :refer [useDndMonitor useDraggable DndContext]
             :rename {DndContext dnd-context
                      useDndMonitor use-dnd-monitor
                      useDraggable use-draggable}]))

(def ^:private note-icons
  ["journal-bookmark-fill" "dice-5" "door-open" "geo-alt" "fire" "skull" "question-circle"])

(defn ^:private resize-cursor [deg]
  (cond
    (> deg 345) "ew-resize"
    (> deg 285) "nesw-resize"
    (> deg 255) "ns-resize"
    (> deg 195) "nwse-resize"
    (> deg 165) "ew-resize"
    (> deg 105) "nesw-resize"
    (> deg 75)  "ns-resize"
    (> deg 15)  "nwse-resize"
    (> deg 0)   "ew-resize"))

(defn ^:private stop-propagation
  "Defines an event handler that ceases event propagation."
  [event]
  (.stopPropagation event))

(defn ^:private compare-objects
  "Defines a comparator function for shapes."
  [a b]
  (let [{point-a :object/point} a
        {point-b :object/point} b]
    (compare
     [(.-x point-a) (.-y point-a)]
     [(.-x point-b) (.-y point-b)])))

(defn ^:private compare-tokens
  "Defines a comparator function for tokens."
  [a b]
  (let [{size-a :token/size point-a :object/point} a
        {size-b :token/size point-b :object/point} b]
    (compare
     [size-b (.-x point-a) (.-y point-a)]
     [size-a (.-x point-b) (.-y point-b)])))

(def ^{:private true
       :doc "Defines a transducer which expects a collection of user
             entities, returning a sequence of key-value pairs whose
             keys are the IDs of the objects currently being dragged
             and whose values are the user that is dragging that object."}
  user-drag-xf
  (comp (filter (comp nil? :db/ident))
        (filter (comp seq :user/dragging))
        (mapcat (fn [user]
                  (map (juxt :db/id (constantly user))
                       (:user/dragging user))))))

(defn ^:private object-authority?
  "Whether the local viewer has visibility authority over `entity` while
   it's hidden -- i.e. whether resolve-hidden shows them the real
   content instead of the placeholder -- the host by default, or its
   assigned player's connected controller once assigned (see
   ogres.app.player/authority?).

   Deliberately does NOT consider :object/shared? here, even though
   authorized-to-hide? (events.cljs) and this file's own action-hide
   disabled-checks DO treat :object/shared? as an authorization
   escape hatch -- those two concerns are different questions.
   :object/shared? means 'anyone connected may TOGGLE this object's
   hidden state' (e.g. any player may flip a physical card on their
   turn), NOT 'anyone may see its real content while it's still
   hidden.' Folding shared? in here would mean a still-face-down
   Memory card renders its true value to every viewer immediately
   (since (or authorized? (not hidden)) would already be true before
   anyone ever flips it) -- defeating the entire 'face-down until
   flipped' mechanic. Once an object IS flipped (:object/hidden false),
   resolve-hidden's OTHER clause already shows it to everyone
   regardless of authority, so :object/shared? still fully achieves
   'a public reveal, visible to everyone the instant any authorized
   participant flips it' -- it just doesn't grant early/automatic
   visibility before that flip happens."
  [viewer-uuid host? connected-uuids entity]
  (player/authority? viewer-uuid host? connected-uuids
                      (get-in entity [:object/owner :player/controller :user/uuid])))

(defn ^:private interactable?
  "Whether the local viewer may select/interact with `entity` despite it
   normally being locked-for-players -- :object/shared? true (ANY
   connected participant may interact with a shared object, e.g. to
   reach its hide/reveal control -- selecting it is a prerequisite for
   that) OR object-authority? (the object's owner/controlling player).
   Deliberately broader than object-authority? alone: that function
   excludes :object/shared? on purpose (see its own docstring --
   visibility-while-hidden and selectability are different questions),
   but selection-lock is the one place both authorization paths
   converge, since flipping something first requires selecting it."
  [viewer-uuid host? connected-uuids entity]
  (or (:object/shared? entity)
      (object-authority? viewer-uuid host? connected-uuids entity)))

;; Memory cards used to be scene props flagged here so a click could
;; flip them; they are now drawn inside their own :minigame/table and
;; carry :data-memory-hidden themselves (see minigame-card), so the
;; scene-object level no longer knows anything about Memory.

(defn ^:private resolve-hidden
  "Resolves how a token/prop entity should render given whether the
   viewer is `authorized?` for it: authorized, or not hidden -- render
   normally, unchanged. Hidden and unauthorized, with a placeholder
   image set (`alt-key`, e.g. :token/image-alt) -- still render, at its
   normal position, with `image-key`'s value swapped for the
   placeholder's (reads as a face-down card, not a ghost). Hidden,
   unauthorized, no placeholder set -- nil (dropped entirely), the same
   fully-invisible behavior as before this existed."
  [entity authorized? image-key alt-key]
  (cond
    (or authorized? (not (:object/hidden entity))) entity
    (some? (get entity alt-key)) (assoc entity image-key (get entity alt-key))
    :else nil))

(defn tokens-xf
  "Defines a transducer which expects a collection of token entities and
   returns only the elements suitable for rendering given the viewer's
   authority over each -- see object-authority?/resolve-hidden.

   Public because component/scene's `tokens-defs` must resolve tokens
   through the EXACT same rule this file's `<use>` list does. The two
   render halves are split -- the artwork and badges live in a
   `<g id=\"token<id>\">` def over there, referenced by a `<use>` here --
   so filtering only on this side would leave a hidden token's real face
   sitting in the defs for anyone to see."
  [viewer-uuid host? connected-uuids]
  (keep
   (fn [token]
     (resolve-hidden token (object-authority? viewer-uuid host? connected-uuids token)
                     :token/image :token/image-alt))))

(defn ^:private use-cursor-point
  "Defines a React state hook which returns a point [Ax Ay] of the
   given user's current cursor position, if available."
  [uuid point]
  (let [[cursor set-cursor] (uix/use-state nil)]
    (uix/use-effect
     (fn []
       (if (nil? uuid)
         (set-cursor nil))) [uuid])
    (hooks/use-subscribe :cursor/moved
      (uix/use-callback
       (fn [id cx cy]
         (if (= id uuid)
           (set-cursor
            (fn [[_ _ dx dy]]
              (let [rx (- cx (.-x point)) ry (- cy (.-y point))]
                (if (nil? dx)
                  [(- rx rx) (- ry ry) rx ry]
                  [(- cx (.-x point) dx) (- cy (.-y point) dy) dx dy])))))) [uuid point]))
    cursor))

(defui ^:private drag-remote-fn
  "Renders the given children as a function with the user's current
   cursor position as its only argument in the form of [Ax Ay]."
  [{:keys [children user point]}]
  (let [point (use-cursor-point user point)]
    (if (nil? point)
      (children nil)
      (children (Vec2. (point 0) (point 1))))))

(defui ^:private drag-local-fn
  "Renders the given children as a function with an object of drag
   parameters passed as its only argument.
   https://docs.dndkit.com/api-documentation/draggable/usedraggable"
  [{:keys [children id disabled]
    :or   {disabled false}}]
  (let [options (use-draggable #js {"id" id "disabled" (boolean disabled)})]
    (children options)))

(defui ^:private shape-circle [props]
  (let [{{[dst] :shape/points} :entity} props]
    ($ :circle.scene-shape-fill
      {:r (vec/dist-cheb dst)})))

(defui ^:private shape-rect [props]
  (let [{{[dst] :shape/points} :entity} props]
    ($ :path.scene-shape-fill
      {:d (join " " [\M 0 0 \H (.-x dst) \V (.-y dst) \H 0 \Z])})))

(defui ^:private shape-line [props]
  (let [{{[dst] :shape/points} :entity} props]
    ($ :polygon.scene-shape-fill
      {:points (->> (Segment. vec/zero dst) (geom/line-points) (mapcat seq) (join " "))})))

(defui ^:private shape-cone [props]
  (let [{{[dst] :shape/points} :entity} props]
    ($ :polygon.scene-shape-fill
      {:points (->> (Segment. vec/zero dst) (geom/cone-points) (mapcat seq) (join " "))})))

(defui ^:private shape-poly [props]
  (let [{{points :shape/points} :entity} props]
    ($ :polygon.scene-shape-fill
      {:points (join " " (into [0 0] (mapcat seq) points))})))

(defui ^:private shape [props]
  (case (:object/type (:entity props))
    :shape/circle ($ shape-circle props)
    :shape/rect   ($ shape-rect props)
    :shape/line   ($ shape-line props)
    :shape/cone   ($ shape-cone props)
    :shape/poly   ($ shape-poly props)
    nil))

(defui ^:private object-shape [props]
  (let [{{id :db/id shape-pattern :shape/pattern :as entity} :entity} props]
    ($ :g.scene-shape {:data-color (:shape/color entity)}
      ($ :defs.scene-shape-defs
        ($ pattern {:id (str "shape-pattern-" id) :name shape-pattern}))
      (if (not= (:object/type entity) :shape/rect)
        (let [bounds (geom/object-bounding-rect entity)]
          ($ :rect.scene-shape-bounds
            {:width (seg/width bounds)
             :height (seg/height bounds)
             :transform (vec/sub (.-a bounds) (:object/point entity))})))
      ($ :g.scene-shape-path
        {:fill (str "url(#shape-pattern-" id ")")
         :fill-opacity (if (= shape-pattern :solid) 0.40 0.80)}
        ($ shape props)))))

(defui ^:private object-token [props]
  ($ :use
    {:href (str "#token" (:db/id (:entity props)))}))

(defui ^:private object-note [props]
  (let [dispatch  (hooks/use-dispatch)
        entity    (:entity props)
        id        (:db/id entity)
        hidden    (:object/hidden entity)
        camera    (first (:camera/_selected entity))
        selected  (into #{} (map :db/id) (:camera/selected camera))
        selected? (and (contains? camera :user/_camera) (= selected #{id}))]
    ($ :foreignObject.scene-object-note
      {:x -8 :y -8 :width 362 :height (if selected? 334 58) :data-selected selected?}
      ($ :.scene-note {:data-hidden hidden}
        ($ :.scene-note-header
          ($ :.scene-note-anchor
            ($ icon {:name (:note/icon entity) :size 26}))
          ($ :.scene-note-nav
            ($ :.scene-note-navinner
              ($ :.scene-note-label
                (let [label (:note/label entity)]
                  (if (and (some? label) (not= label ""))
                    label "Unlabeled note")))
              ($ :.scene-note-control
                {:on-pointer-down stop-propagation
                 :on-click (fn [] (dispatch :objects/toggle-hidden id))}
                ($ icon {:name (if hidden "eye-slash-fill" "eye-fill") :size 22}))
              ($ :.scene-note-control
                {:on-pointer-down stop-propagation
                 :on-click (fn [] (dispatch :objects/remove [id]))}
                ($ icon {:name "trash3-fill" :size 22})))))
        (if selected?
          ($ :.scene-note-body {:on-pointer-down stop-propagation}
            ($ :ul.scene-note-icons
              (for [icon-name note-icons]
                ($ :li {:key icon-name}
                  ($ :label
                    ($ :input
                      {:type "radio"
                       :name "note-icon"
                       :value icon-name
                       :checked (= (:note/icon entity) icon-name)
                       :on-change
                       (fn [event]
                         (dispatch :note/change-icon (:db/id entity) (.. event -target -value)))})
                    ($ icon {:name icon-name})))))
            ($ :form.scene-note-form
              {:on-blur
               (fn [event]
                 (let [name  (.. event -target -name)
                       value (.. event -target -value)]
                   (cond (and (= name "label") (not= (:note/label entity) value))
                         (dispatch :note/change-label id value)
                         (and (= name "description") (not= (:note/description entity) value))
                         (dispatch :note/change-description id value))))
               :on-submit
               (fn [event]
                 (.preventDefault event)
                 (let [input (.. event -target -elements)
                       label (.. input -label -value)
                       descr (.. input -description -value)]
                   (dispatch :note/change-details id label descr)))}
              ($ :fieldset.fieldset
                ($ :legend "Label")
                ($ :input.text.text-ghost
                  {:type "text"
                   :name "label"
                   :auto-complete "off"
                   :default-value (:note/label entity)}))
              ($ :fieldset.fieldset
                ($ :legend "Description")
                ($ :textarea
                  {:name "description"
                   :auto-complete "off"
                   :default-value (:note/description entity)}))
              ($ :input {:type "submit" :hidden true}))))))))

(def ^:private anchor-marker-angles
  "The undirected grid-line angles (degrees) that meet at a single cell
   vertex for each base grid type -- two perpendicular lines for a square
   grid, three lines 60 degrees apart for a hex grid (matching the edge
   directions derived from geom/hex-points / hex-points-flat: pointy-top
   hexes tile with edges at 30/90/150 degrees, flat-top at 0/60/120).
   Drawing the anchor marker as this same axis, rather than a plain dot,
   lets it be visually lined up against the grid lines actually drawn in
   the artwork. Square is offset 45 degrees from a plain edge-crossing
   cross so its arms point at adjacent cell corners (diagonals) instead
   of along the cell edges -- easier to eyeball against a square grid's
   corner points."
  {:square [45 135]
   :hex-pointy [30 90 150]
   :hex-flat [0 60 120]})

(defui ^:private ^:memo object-anchor-marker
  [{:keys [point size grid-type]}]
  (let [option #js {"id" "anchor" "data" #js {"type" "anchor"}}
        drag (use-draggable option)
        handle (and (.-listeners drag) (.-onPointerDown (.-listeners drag)))
        px (.-x point) py (.-y point)
        ;; Each arm spans roughly two grid cells, in the same world-space
        ;; units the grid itself is drawn in (unscaled by the piece's own
        ;; :object/scale or rotation -- this marker is rendered in a
        ;; grid-locked wrapper precisely so it doesn't spin or resize with
        ;; the piece, only the camera's own zoom naturally applies, same
        ;; as the real grid lines).
        len (* 2 grid-size)
        angles (get anchor-marker-angles (geom/base-grid-type grid-type) (:square anchor-marker-angles))]
    ($ :g.scene-object-anchor-target
      {:data-dragging (.-isDragging drag)
       :on-pointer-down handle
       :style {:cursor (if (.-isDragging drag) "grabbing" "grab")}}
      ($ :circle.scene-object-anchor-hit {:cx px :cy py :r (* size 2)})
      (for [deg angles
            :let [rad (* deg (/ js/Math.PI 180))
                  dx (* len (js/Math.cos rad))
                  dy (* len (js/Math.sin rad))]]
        ($ :line.scene-object-anchor-line
          {:key deg
           :x1 (- px dx) :y1 (- py dy)
           :x2 (+ px dx) :y2 (+ py dy)}))
      ($ :circle.scene-object-anchor-dot {:cx px :cy py :r (/ size 2.5)}))))

(def ^:private anchor-nudge-step
  "Native artwork pixels moved per keyboard nudge of the grid-anchor
   marker (arrow keys or numpad directions) -- a small, fixed, zoom- and
   scale-independent step, since :image/anchor is itself stored in the
   image's own native pixel space."
  1)

(def ^:private anchor-nudge-arrow-angles
  {"ArrowRight" 0 "ArrowDown" 90 "ArrowLeft" 180 "ArrowUp" 270})

(def ^:private anchor-nudge-hex-pointy-angles
  "Numpad-direction angles (screen degrees, clockwise from due east) for a
   pointy-top hex grid's 6 neighbor-center directions -- pointy-top hexes
   have flat left/right sides, so they get direct west/east neighbors
   (4/6) plus the four 60-degree-apart diagonals (7/9/1/3). Derived from
   vec/nearest-hex's hex-w/row-h lattice spacing, not eyeballed."
  {"Numpad6" 0 "Numpad3" 60 "Numpad1" 120 "Numpad4" 180 "Numpad7" 240 "Numpad9" 300})

(def ^:private anchor-nudge-hex-flat-angles
  "Same idea as anchor-nudge-hex-pointy-angles, but for a flat-top hex
   grid -- flat-top hexes have flat top/bottom edges instead, so they get
   direct north/south neighbors (8/2) plus the same four diagonals
   (7/9/1/3), each rotated 30 degrees from the pointy-top set."
  {"Numpad3" 30 "Numpad2" 90 "Numpad1" 150 "Numpad7" 210 "Numpad8" 270 "Numpad9" 330})

(def ^:private anchor-nudge-codes
  "Every key code the grid-anchor nudge handler cares about, regardless of
   grid-type -- used to swallow these keys outright while anchor-editing,
   even on a code/grid-type combination anchor-nudge-angle itself treats
   as a no-op (e.g. Numpad8 on a pointy-top hex grid), so they never leak
   through to dnd-kit's own keyboard handling on the scene's selection
   draggable."
  #{"ArrowUp" "ArrowDown" "ArrowLeft" "ArrowRight"
    "Numpad1" "Numpad2" "Numpad3" "Numpad4" "Numpad6" "Numpad7" "Numpad8" "Numpad9"})

(defn ^:private anchor-nudge-angle
  "The screen-space angle (degrees) the given key code should nudge the
   grid-anchor marker along, or nil if that code isn't a nudge key for
   this grid-type. Plain arrows always work; the numpad directions are
   hex-family-specific (see the two tables above) since a square grid's
   anchor only ever needs to move along its own square axis, which the
   arrow keys already cover."
  [grid-type code]
  (or (get anchor-nudge-arrow-angles code)
      (case (geom/base-grid-type grid-type)
        :hex-pointy (get anchor-nudge-hex-pointy-angles code)
        :hex-flat (get anchor-nudge-hex-flat-angles code)
        nil)))

(defn ^:private scale-locked?
  "True when `entity`'s dimensions must not change. A mini-game table
   locks while it still holds cards: resizing mid-game would rescale the
   board under the players. It unlocks once the game is over (every pair
   matched, so no cards remain) and the table is just an empty frame
   waiting to be cleared."
  [entity]
  (and (= (:object/type entity) :minigame/table)
       (seq (:minigame/cards entity))))

(defui ^:private ^:memo object-prop-scale
  [{:keys [point size angle]}]
  (let [option #js {"id" (str "resize/" point) "data" #js {"type" "resize" "point" point}}
        resize (use-draggable option)
        handle (and (.-listeners resize) (.-onPointerDown (.-listeners resize)))
        cursor (resize-cursor angle)]
    ($ :<>
      (if (.-isDragging resize)
        (dom/create-portal
         ($ :.cursor-region
           {:style {:cursor cursor}}) js/document.body))
      ($ :rect.scene-prop-anchor
        {:data-dragging (.-isDragging resize)
         :on-pointer-down handle
         :style {:cursor cursor}
         :x (- (.-x point) (/ size 2))
         :y (- (.-y point) (/ size 2))
         :width size
         :height size}))))

(defui ^:private ^:memo object-prop-rotate
  [{:keys [point size]}]
  (let [option #js {"id" "rotate" "data" #js {"type" "rotate" "point" point}}
        rotate (use-draggable option)
        handle (and (.-listeners rotate) (.-onPointerDown (.-listeners rotate)))
        cursor (if (.-isDragging rotate) "grabbing" "grab")]
    ($ :<>
      (if (.-isDragging rotate)
        (dom/create-portal
         ($ :.cursor-region
           {:style {:cursor "grabbing"}}) js/document.body))
      ($ :circle.scene-prop-anchor
        {:data-dragging (.-isDragging rotate)
         :on-pointer-down handle
         :style {:cursor cursor}
         :cx (.-x point)
         :cy (.-y point)
         :r size}))))

(defui ^:private object-prop-edit [props]
  (let [{{id :db/id
          object-scale :object/scale
          object-rotation :object/rotation
          {width :image/width
           height :image/height
           anchor :image/anchor} :prop/image
          [{zoom :camera/scale draw-mode :camera/draw-mode}] :camera/_selected} :entity
         transform :transform
         grid-type :grid-type} props
        default-anchor (Vec2. (/ width 2) (/ height 2))
        [scale set-scale] (uix/use-state object-scale)
        [rotation set-rotation] (uix/use-state object-rotation)
        [anchor-point set-anchor-point] (uix/use-state (or anchor default-anchor))
        [anchor-origin set-anchor-origin] (uix/use-state (or anchor default-anchor))
        dispatch (hooks/use-dispatch)
        bounds (Segment. vec/zero (Vec2. width height))
        center (seg/midpoint bounds)
        anchor-editing? (= draw-mode :object-anchor)
        get-scale
        (fn [^js/Object event]
          (let [data (.. event -active -data -current)
                dx (.-x (.-delta event))
                dy (.-y (.-delta event))]
            (-> (vec/shift (transform (.-point data)) dx dy)
                (vec/dist center)
                (/ (vec/dist center)))))
        get-rotation
        (fn [^js/Object event]
          (let [data (.. event -active -data -current)
                dx (.-x (.-delta event))
                dy (.-y (.-delta event))
                dg (-> (vec/shift (transform (.-point data)) dx dy)
                       (vec/sub center)
                       (vec/angle)
                       (+ 90))
                rd (util/round dg 45)]
            (if (< (abs (- dg rd)) 5) rd dg)))
        get-anchor-point
        (fn [^js/Object event]
          (let [dx (.-x (.-delta event))
                dy (.-y (.-delta event))]
            ((matrix/inverse transform) (vec/shift (transform anchor-origin) dx dy))))]
    (uix/use-effect
     (fn []
       (set-scale object-scale)
       (set-rotation object-rotation))
     [object-scale object-rotation])
    (use-dnd-monitor
     #js {"onDragStart"
          (fn [event]
            (case (.. event -active -data -current -type)
              "anchor" (set-anchor-origin anchor-point)
              nil))
          "onDragMove"
          (fn [event]
            (case (.. event -active -data -current -type)
              "resize" (set-scale (get-scale event))
              "rotate" (set-rotation (get-rotation event))
              "anchor" (set-anchor-point (get-anchor-point event))
              nil))
          "onDragEnd"
          (fn [event]
            (case (.. event -active -data -current -type)
              "resize" (dispatch :object/change-scale id (get-scale event))
              "rotate" (dispatch :object/change-rotation id (get-rotation event))
              "anchor" (set-anchor-point (get-anchor-point event))
              nil))})
    ;; Keyboard nudging while placing the grid anchor -- see the identical
    ;; effect in object-board-edit for the full rationale (window-level
    ;; capture-phase keydown, event.code numpad detection, and why the
    ;; nudge has to round-trip through world space via get-anchor-point's
    ;; same transform instead of applying dx/dy to the local point
    ;; directly).
    (uix/use-effect
     (fn []
       (if anchor-editing?
         (let [handler
               (fn [^js/Object event]
                 (let [code (.-code event)]
                   (cond
                     (contains? anchor-nudge-codes code)
                     (let [angle (anchor-nudge-angle grid-type code)]
                       (.preventDefault event)
                       (.stopPropagation event)
                       (if (number? angle)
                         (let [rad (* angle (/ js/Math.PI 180))
                               dx (* anchor-nudge-step (js/Math.cos rad))
                               dy (* anchor-nudge-step (js/Math.sin rad))]
                           (set-anchor-point
                            (fn [current]
                              ((matrix/inverse transform)
                               (vec/shift (transform current) dx dy)))))))

                     (= code "Enter")
                     (do
                       (.preventDefault event)
                       (.stopPropagation event)
                       (dispatch :image/set-anchor id anchor-point)
                       (dispatch :camera/change-mode :select))

                     (= code "Escape")
                     (do
                       (.preventDefault event)
                       (.stopPropagation event)
                       (dispatch :camera/change-mode :select)))))]
           (js/window.addEventListener "keydown" handler true)
           (fn [] (js/window.removeEventListener "keydown" handler true)))))
     [anchor-editing? grid-type transform anchor-point id dispatch])
    ($ :<>
      ($ :g.scene-prop
        {:style
         {:transform
          (-> (matrix/translate matrix/identity center)
              (matrix/scale scale)
              (matrix/rotate rotation)
              (matrix/translate (vec/mul center -1)))}}
        (:children props)
        (if-not anchor-editing?
          ($ :<>
            (for [point (geom/rect-points bounds)]
              ($ object-prop-scale
                {:key point
                 :point point
                 :size (/ 8 scale zoom)
                 :angle (vec/angle (vec/sub (transform point) center))}))
            ($ object-prop-rotate
              {:point (Vec2. (.-x center) (/ 26 scale zoom -1))
               :size (/ 5 scale zoom)}))))
      (if anchor-editing?
        (let [world (transform anchor-point)
              r (/ 12 zoom)
              gap (/ 30 zoom)
              bx (/ 26 zoom)
              by (- (/ 26 zoom))
              isz (/ 14 zoom)]
          ($ :g {:style {:transform (str "translate(" (.-x world) "px, " (.-y world) "px)")}}
            ($ object-anchor-marker
              {:point vec/zero :size (/ 10 zoom) :grid-type grid-type})
            ($ :g.scene-object-anchor-confirm
              {:on-pointer-down
               stop-propagation
               :on-click
               (fn []
                 (dispatch :image/set-anchor id anchor-point)
                 (dispatch :camera/change-mode :select))}
              ($ :title "Save anchor")
              ($ :circle {:cx bx :cy by :r r})
              ($ :g {:transform (str "translate(" (- bx (/ isz 2)) "," (- by (/ isz 2)) ")")}
                ($ icon {:name "check" :size isz})))
            ($ :g.scene-object-anchor-cancel
              {:on-pointer-down stop-propagation
               :on-click #(dispatch :camera/change-mode :select)}
              ($ :title "Cancel")
              ($ :circle {:cx (+ bx gap) :cy by :r r})
              ($ :g {:transform (str "translate(" (- (+ bx gap) (/ isz 2)) "," (- by (/ isz 2)) ")")}
                ($ icon {:name "x" :size isz})))))))))

(defui ^:private minigame-card-back
  "A face-down card, drawn inline rather than loaded as an image. Keeping
   it here means a table depends on no seeded prop asset at all -- the
   last thread tying a mini-game to the main game's image gallery. The
   motif is a plain lattice: generic enough for any card game, and
   readable at the size a whole 8x6 board is usually viewed at."
  []
  ($ :<>
    ($ :rect.scene-minigame-card-face
      {:width memory/card-width :height memory/card-height :rx 14})
    ($ :rect.scene-minigame-card-frame
      {:x 12 :y 12 :width (- memory/card-width 24) :height (- memory/card-height 24) :rx 8})
    ($ :rect.scene-minigame-card-lattice
      {:x 12 :y 12 :width (- memory/card-width 24) :height (- memory/card-height 24)
       :rx 8 :fill "url(#minigame-card-lattice)"})
    ;; The four suits, reusing the same symbols the card panels use
    ;; rather than redrawing them -- a generic playing-card back, not a
    ;; Memory-specific one. Memory's own cards have no suit (they pair
    ;; by number); this is decoration on the FACE-DOWN side only.
    ($ :g.scene-minigame-card-motif
      {:transform (str "translate(" (- (/ memory/card-width 2) 44) ", "
                       (- (/ memory/card-height 2) 44) ")")}
      (for [[dx dy nm] [[0 0 "suit-spade-fill"] [44 0 "suit-heart-fill"]
                        [0 44 "suit-diamond-fill"] [44 44 "suit-club-fill"]]]
        ($ :g {:key nm :transform (str "translate(" dx ", " dy ")")}
          ($ icon {:name nm :size 44}))))))

(def ^:private suit-icon-names
  {:clubs "suit-club-fill" :diamonds "suit-diamond-fill"
   :hearts "suit-heart-fill" :spades "suit-spade-fill"})

(defui ^:private minigame-card-face
  "A face-up card, drawn from its rank and suit rather than loaded as an
   image -- corner index top-left and bottom-right (rotated, as on a real
   card) plus a large centre pip. Red for hearts and diamonds, black for
   clubs and spades; the colour is half of what makes a pair, so it has
   to be legible at a glance across a 52-card board."
  [{card :card}]
  (let [suit (:card/suit card)
        rank (:card/rank card)
        label (get card-pile/rank-short rank)
        icon-name (get suit-icon-names suit)]
    ($ :g.scene-minigame-card-face-group
      {:data-color (name (memory/suit-color suit))}
      ($ :rect.scene-minigame-card-face
        {:width memory/card-width :height memory/card-height :rx 14})
      ($ :text.scene-minigame-card-index {:x 20 :y 52} label)
      ($ :g {:transform "translate(14, 60)"}
        ($ icon {:name icon-name :size 30}))
      ($ :g {:transform (str "translate(" (- (/ memory/card-width 2) 45) ", "
                             (- (/ memory/card-height 2) 45) ")")}
        ($ icon {:name icon-name :size 90}))
      ;; the far corner repeats the index upside-down, as a real card does
      ($ :g {:transform (str "rotate(180, " (/ memory/card-width 2) ", "
                             (/ memory/card-height 2) ")")}
        ($ :text.scene-minigame-card-index {:x 20 :y 52} label)
        ($ :g {:transform "translate(14, 60)"}
          ($ icon {:name icon-name :size 30}))))))

(defui ^:private minigame-card [props]
  (let [{card :card} props
        face-up? (:memory/face-up? card)]
    ($ :g.scene-minigame-card
      {:transform (memory/card-offset (:memory/index card))
       ;; Each card carries its own data-id, so use-drag-listener's
       ;; zero-delta (click) branch resolves to the CARD while a real
       ;; drag still bubbles to the table group and moves the whole
       ;; board. Same closest("[data-id]") mechanism the loose props
       ;; used, without the cards being scene objects.
       :data-id (:db/id card)
       :data-memory-hidden (not face-up?)}
      (if face-up?
        ($ minigame-card-face {:card card})
        ($ minigame-card-back))
      ($ :rect.scene-minigame-card-bounds
        {:width memory/card-width :height memory/card-height :rx 14}))))

(defui ^:private minigame-table-content [props]
  (let [{cards :minigame/cards} (:entity props)
        [w h] (memory/table-footprint)]
    ($ :<>
      ($ :defs
        ;; One lattice tile, reused by every card back on this table.
        ($ :pattern
          {:id "minigame-card-lattice" :width 24 :height 24
           :patternUnits "userSpaceOnUse"}
          ($ :path.scene-minigame-card-lattice-path
            {:d "M0,12 L12,0 L24,12 L12,24 Z"})))
      ;; The felt. Drawn first so it sits under the cards, and sized from
      ;; the same footprint the bounding rect uses -- what you see is
      ;; exactly what you grab.
      ($ :rect.scene-minigame-felt {:width w :height h :rx 24})
      (for [card (sort-by :memory/index cards)]
        ($ minigame-card {:key (:db/id card) :card card})))))

;; Scale handles for a table, mirroring object-prop-edit but scale-only:
;; a table has no rotation and no image anchor to place, and its felt is
;; drawn from a fixed footprint rather than an image's dimensions.
(defui ^:private object-minigame-table-edit [props]
  (let [{{id :db/id
          object-scale :object/scale
          [{zoom :camera/scale}] :camera/_selected} :entity
         transform :transform} props
        [w h] (memory/table-footprint)
        [scale set-scale] (uix/use-state object-scale)
        dispatch (hooks/use-dispatch)
        bounds (Segment. vec/zero (Vec2. w h))
        center (seg/midpoint bounds)
        get-scale
        (fn [^js/Object event]
          (let [data (.. event -active -data -current)
                dx (.-x (.-delta event))
                dy (.-y (.-delta event))]
            (-> (vec/shift (transform (.-point data)) dx dy)
                (vec/dist center)
                (/ (vec/dist center)))))]
    (uix/use-effect
     (fn [] (set-scale object-scale)) [object-scale])
    (use-dnd-monitor
     #js {"onDragMove"
          (fn [event]
            (case (.. event -active -data -current -type)
              "resize" (set-scale (get-scale event))
              nil))
          "onDragEnd"
          (fn [event]
            (case (.. event -active -data -current -type)
              "resize" (dispatch :object/change-scale id (get-scale event))
              nil))})
    ($ :g.scene-minigame-table
      {:style
       {:transform
        (-> (matrix/translate matrix/identity center)
            (matrix/scale scale)
            (matrix/translate (vec/mul center -1)))}}
      (:children props)
      (for [point (geom/rect-points bounds)]
        ($ object-prop-scale
          {:key point
           :point point
           :size (/ 8 scale zoom)
           :angle (vec/angle (vec/sub (transform point) center))})))))

(defui ^:private object-minigame-table [props]
  (let [{{id :db/id
          [{selected :camera/selected
            [{user :root/_user}] :user/_camera
            zoom :camera/scale
            {grid-type :scene/grid-type} :camera/scene}] :camera/_selected} :entity} props
        mod-scale (uix/use-memo (fn [] (modifiers/scale-fn zoom grid-type)) [zoom grid-type])
        transform (geom/object-transform (:entity props))
        selected (into #{} (map :db/id) selected)]
    ;; A table is movable whenever it is placed, but its dimensions lock
    ;; the moment a game is dealt onto it -- resizing mid-game would
    ;; rescale the board out from under the players (see scale-locked?).
    (if (and (some? user)
             (not (scale-locked? (:entity props)))
             (= #{id} selected))
      ($ dnd-context
        #js {"modifiers" #js [mod-scale modifiers/trunc]}
        ($ object-minigame-table-edit
          (assoc props :transform transform)
          ($ minigame-table-content props)))
      ($ :g.scene-minigame-table {:style {:transform transform}}
        ($ minigame-table-content props)))))

(defui ^:private object-prop [props]
  (let [{{id :db/id
          hidden :object/hidden
          locked :object/locked
          {hash :image/hash
           width :image/width
           height :image/height} :prop/image
          [{selected :camera/selected
            [{user :root/_user}] :user/_camera
            zoom :camera/scale
            {grid-type :scene/grid-type} :camera/scene}] :camera/_selected} :entity} props
        url-image (hooks/use-image hash)
        mod-scale (uix/use-memo (fn [] (modifiers/scale-fn zoom grid-type)) [zoom grid-type])
        transform (geom/object-transform (:entity props))
        selected (into #{} (map :db/id) selected)]
    (if (and (some? user) (not locked) (= #{id} selected))
      ($ dnd-context
        #js {"modifiers" #js [mod-scale modifiers/trunc]}
        ($ object-prop-edit
          (assoc props :transform transform)
          ($ :image.scene-prop-image
            {:data-hidden hidden
             :width width
             :height height
             :href url-image})
          ($ :rect.scene-prop-bounds
            {:width width :height height})))
      ($ :g.scene-prop {:style {:transform transform}}
        ($ :image.scene-prop-image
          {:data-hidden hidden
           :width width
           :height height
           :href url-image})
        ($ :rect.scene-prop-bounds
          {:width width :height height})))))

;; Board pieces render exactly like props (same scale/rotate handles,
;; reusing object-prop-scale/object-prop-rotate as-is since they're
;; already generic) and reuse the same .scene-prop* CSS classes -- the
;; only real differences are reading :board/image instead of :prop/image,
;; and hard-snapping rotation drags to the piece's own
;; :object/rotation-mode instead of the soft 45-degree assist props use.
;; Locking already means "no edit handles at all" for props (see
;; object-prop's (not locked) check above, which fully replaces rendering
;; with the plain non-editable branch) -- board pieces get that same
;; behavior for free by mirroring the same structure, satisfying "locked
;; board pieces only have their visibility left togglable" without any
;; extra lock-strength logic of their own.
(defui ^:private object-board-edit [props]
  (let [{{id :db/id
          object-scale :object/scale
          object-rotation :object/rotation
          rotation-mode :object/rotation-mode
          {width :image/width
           height :image/height
           anchor :image/anchor} :board/image
          [{zoom :camera/scale draw-mode :camera/draw-mode}] :camera/_selected} :entity
         transform :transform
         grid-type :grid-type} props
        default-anchor (Vec2. (/ width 2) (/ height 2))
        [scale set-scale] (uix/use-state object-scale)
        [rotation set-rotation] (uix/use-state object-rotation)
        [anchor-point set-anchor-point] (uix/use-state (or anchor default-anchor))
        [anchor-origin set-anchor-origin] (uix/use-state (or anchor default-anchor))
        dispatch (hooks/use-dispatch)
        bounds (Segment. vec/zero (Vec2. width height))
        center (seg/midpoint bounds)
        anchor-editing? (= draw-mode :object-anchor)
        get-scale
        (fn [^js/Object event]
          (let [data (.. event -active -data -current)
                dx (.-x (.-delta event))
                dy (.-y (.-delta event))]
            (-> (vec/shift (transform (.-point data)) dx dy)
                (vec/dist center)
                (/ (vec/dist center)))))
        get-rotation
        (fn [^js/Object event]
          (let [data (.. event -active -data -current)
                dx (.-x (.-delta event))
                dy (.-y (.-delta event))
                dg (-> (vec/shift (transform (.-point data)) dx dy)
                       (vec/sub center)
                       (vec/angle)
                       (+ 90))]
            (if (number? rotation-mode) (util/round dg rotation-mode) dg)))
        get-anchor-point
        (fn [^js/Object event]
          (let [dx (.-x (.-delta event))
                dy (.-y (.-delta event))]
            ((matrix/inverse transform) (vec/shift (transform anchor-origin) dx dy))))]
    (uix/use-effect
     (fn []
       (set-scale object-scale)
       (set-rotation object-rotation))
     [object-scale object-rotation])
    (use-dnd-monitor
     #js {"onDragStart"
          (fn [event]
            (case (.. event -active -data -current -type)
              "anchor" (set-anchor-origin anchor-point)
              nil))
          "onDragMove"
          (fn [event]
            (case (.. event -active -data -current -type)
              "resize" (set-scale (get-scale event))
              "rotate" (set-rotation (get-rotation event))
              "anchor" (set-anchor-point (get-anchor-point event))
              nil))
          "onDragEnd"
          (fn [event]
            (case (.. event -active -data -current -type)
              "resize" (dispatch :object/change-scale id (get-scale event))
              "rotate" (dispatch :object/change-rotation id (get-rotation event))
              "anchor" (set-anchor-point (get-anchor-point event))
              nil))})
    ;; Keyboard nudging while placing the grid anchor -- bound directly to
    ;; window keydown (rather than hooks/use-shortcut) so it can read
    ;; event.code straight off the native KeyboardEvent, which is what
    ;; reliably distinguishes the physical numpad keys from the top-row
    ;; digits regardless of NumLock state; hooks/use-shortcut's "@Code"
    ;; alias matching (see @rwh/keystrokes) doesn't fire in this app's
    ;; setup, so this bypasses that layer for just this one feature.
    (uix/use-effect
     (fn []
       (if anchor-editing?
         (let [handler
               (fn [^js/Object event]
                 (let [code (.-code event)]
                   (cond
                     (contains? anchor-nudge-codes code)
                     (let [angle (anchor-nudge-angle grid-type code)]
                       ;; Stop this event from reaching dnd-kit's own keyboard
                       ;; handling entirely (captured ahead of it, below) --
                       ;; otherwise the scene's "selected" group draggable
                       ;; (which still mounts even though a solo-selected
                       ;; board piece no longer renders through it -- see
                       ;; solo-board? in the objects component) intercepts
                       ;; these same arrow/numpad keys and translates the
                       ;; whole piece by a grid cell, on top of this nudge.
                       (.preventDefault event)
                       (.stopPropagation event)
                       (if (number? angle)
                         (let [rad (* angle (/ js/Math.PI 180))
                               dx (* anchor-nudge-step (js/Math.cos rad))
                               dy (* anchor-nudge-step (js/Math.sin rad))]
                           ;; angle is a grid-locked screen direction (same
                           ;; convention the crosshair itself is drawn in),
                           ;; but anchor-point lives in the piece's own
                           ;; unrotated local space -- so the nudge has to
                           ;; go out to world space, shift there, and come
                           ;; back, exactly like get-anchor-point already
                           ;; does for mouse-drag nudging above. Applying
                           ;; (dx, dy) to the local point directly would
                           ;; make the nudge direction spin with the piece's
                           ;; own :object/rotation instead of staying locked
                           ;; to the grid.
                           (set-anchor-point
                            (fn [current]
                              ((matrix/inverse transform)
                               (vec/shift (transform current) dx dy)))))))

                     ;; Same actions as the on-canvas confirm/cancel buttons
                     ;; (see below) -- Enter/Escape are dnd-kit's own default
                     ;; drag-activation/cancel keys too (see KeyboardSensor's
                     ;; defaultKeyboardCodes), so these are swallowed here
                     ;; the same way the nudge keys are, not just handled.
                     (= code "Enter")
                     (do
                       (.preventDefault event)
                       (.stopPropagation event)
                       (dispatch :image/set-anchor id anchor-point)
                       (dispatch :camera/change-mode :select))

                     (= code "Escape")
                     (do
                       (.preventDefault event)
                       (.stopPropagation event)
                       (dispatch :camera/change-mode :select)))))]
           ;; Registered on the capture phase, ahead of dnd-kit's own
           ;; bubble-phase document listener, so stopPropagation above
           ;; actually keeps it from ever seeing these keys.
           (js/window.addEventListener "keydown" handler true)
           (fn [] (js/window.removeEventListener "keydown" handler true)))))
     [anchor-editing? grid-type transform anchor-point id dispatch])
    ($ :<>
      ($ :g.scene-prop
        {:style
         {:transform
          (-> (matrix/translate matrix/identity center)
              (matrix/scale scale)
              (matrix/rotate rotation)
              (matrix/translate (vec/mul center -1)))}}
        (:children props)
        (if-not anchor-editing?
          ($ :<>
            (for [point (geom/rect-points bounds)]
              ($ object-prop-scale
                {:key point
                 :point point
                 :size (/ 8 scale zoom)
                 :angle (vec/angle (vec/sub (transform point) center))}))
            ($ object-prop-rotate
              {:point (Vec2. (.-x center) (/ 26 scale zoom -1))
               :size (/ 5 scale zoom)}))))
      ;; Rendered as a sibling to (not a descendant of) the piece's own
      ;; scale/rotate wrapper above, positioned by translate-only at the
      ;; anchor's world point -- so the crosshair and confirm/cancel
      ;; controls stay locked to the grid's own orientation (no rotation)
      ;; regardless of the piece's current :object/rotation, which is the
      ;; whole point of using them to line the anchor up against the grid.
      (if anchor-editing?
        (let [world (transform anchor-point)
              r (/ 12 zoom)
              gap (/ 30 zoom)
              bx (/ 26 zoom)
              by (- (/ 26 zoom))
              isz (/ 14 zoom)]
          ($ :g {:style {:transform (str "translate(" (.-x world) "px, " (.-y world) "px)")}}
            ($ object-anchor-marker
              {:point vec/zero :size (/ 10 zoom) :grid-type grid-type})
            ($ :g.scene-object-anchor-confirm
              {:on-pointer-down
               stop-propagation
               :on-click
               (fn []
                 (dispatch :image/set-anchor id anchor-point)
                 (dispatch :camera/change-mode :select))}
              ($ :title "Save anchor")
              ($ :circle {:cx bx :cy by :r r})
              ($ :g {:transform (str "translate(" (- bx (/ isz 2)) "," (- by (/ isz 2)) ")")}
                ($ icon {:name "check" :size isz})))
            ($ :g.scene-object-anchor-cancel
              {:on-pointer-down stop-propagation
               :on-click #(dispatch :camera/change-mode :select)}
              ($ :title "Cancel")
              ($ :circle {:cx (+ bx gap) :cy by :r r})
              ($ :g {:transform (str "translate(" (- (+ bx gap) (/ isz 2)) "," (- by (/ isz 2)) ")")}
                ($ icon {:name "x" :size isz})))))))))

(defui ^:private object-board [props]
  (let [{{id :db/id
          hidden :object/hidden
          locked :object/locked
          {hash :image/hash
           width :image/width
           height :image/height} :board/image
          [{selected :camera/selected
            [{user :root/_user}] :user/_camera
            zoom :camera/scale
            {grid-type :scene/grid-type} :camera/scene}] :camera/_selected} :entity} props
        url-image (hooks/use-image hash)
        mod-scale (uix/use-memo (fn [] (modifiers/scale-fn zoom grid-type)) [zoom grid-type])
        transform (geom/object-transform (:entity props))
        selected (into #{} (map :db/id) selected)]
    (if (and (some? user) (not locked) (= #{id} selected))
      ($ dnd-context
        #js {"modifiers" #js [mod-scale modifiers/trunc]}
        ($ object-board-edit
          (assoc props :transform transform)
          ($ :image.scene-prop-image
            {:data-hidden hidden
             :width width
             :height height
             :href url-image})
          ($ :rect.scene-prop-bounds
            {:width width :height height})))
      ($ :g.scene-prop {:style {:transform transform}}
        ($ :image.scene-prop-image
          {:data-hidden hidden
           :width width
           :height height
           :href url-image})
        ($ :rect.scene-prop-bounds
          {:width width :height height})))))

(def ^:private asset-object-types
  "Object types whose own artwork must render undistorted on an isometric
   scene -- tokens, notes, and props are all user-facing assets (images,
   icons, UI-like widgets), unlike shapes, which represent board-plane
   geometry and are meant to deform along with the projected grid."
  #{:token/token :note/note :prop/prop})

(defui ^:private object [props]
  (let [type (:object/type (:entity props))
        grid-type (:grid-type props)
        content (case type
                  :token/token ($ object-token props)
                  :note/note ($ object-note props)
                  :prop/prop ($ object-prop props)
                  :minigame/table ($ object-minigame-table props)
                  ;; Board pieces are the ground/map plane itself -- like
                  ;; shapes (the default branch below), they're meant to
                  ;; deform along with the projected grid on an isometric
                  ;; scene, not stay crisp/undistorted like a token's face
                  ;; or a UI icon, so :board/piece is deliberately absent
                  ;; from asset-object-types below.
                  :board/piece ($ object-board props)
                  ($ object-shape props))]
    (if (and (geom/iso? grid-type) (contains? asset-object-types type))
      ;; Counter-transform the asset's own content with the isometric
      ;; inverse so its artwork stays undistorted -- no rotation, no
      ;; squish -- while the position translate applied by this object's
      ;; parent (unaffected by this wrapper) still correctly reflects the
      ;; projected board location. Isometric assets, if desired, are the
      ;; table's own art to provide; the framework does not stretch them.
      ($ :g {:style {:transform (geom/iso-inverse-matrix grid-type)}} content)
      content)))

(defn ^:private use-drag-listener []
  (let [dispatch (hooks/use-dispatch)]
    (use-dnd-monitor
     #js {"onDragStart"
          (uix/use-callback
           (fn [data]
             (let [id (.. data -active -id)]
               (if (= id "selected")
                 (dispatch :drag/start-selected)
                 (dispatch :drag/start id)))) [dispatch])
          "onDragCancel"
          (uix/use-callback
           (fn []
             (dispatch :drag/end)) [dispatch])
          "onDragEnd"
          (uix/use-callback
           (fn [data]
             (let [event (.-activatorEvent data)
                   ident (.. data -active -id)
                   delta (Vec2. (.. data -delta -x) (.. data -delta -y))
                   shift (.-shiftKey event)]
               (if (= delta vec/zero)
                 (let [^js node (.. event -target (closest "[data-id]"))
                       id (if (= ident "selected") (js/Number (.. node -dataset -id)) ident)]
                   (if (= "true" (.. node -dataset -memoryHidden))
                     ;; Always the CLICKED node's own id, never the dragged
                     ;; element's: a mini-game card is not itself draggable
                     ;; (its table is), so `ident` here is the table, while
                     ;; the card carries its id on the node closest to the
                     ;; pointer. Using `ident` flipped the table instead of
                     ;; the card and silently did nothing.
                     (dispatch :memory/flip (js/Number (.. node -dataset -id)))
                     (dispatch :objects/select id shift)))
                 (if (= ident "selected")
                   (dispatch :objects/translate-selected delta)
                   (dispatch :objects/translate ident delta))))) [dispatch])})))

(defui ^:private object-hint [props]
  (let [{{point :object/point type :object/type :as entity} :entity
         portal :portal
         delta :delta
         is-outline :is-outline
         is-aligned :is-aligned
         grid-type :grid-type} props
        is-aligning (and is-aligned (not= delta vec/zero))
        base-type (geom/base-grid-type grid-type)]
    ($ :<>
      (if (and (= type :token/token) is-aligning (= base-type :hex-pointy))
        (let [center (vec/nearest-hex (vec/add point delta) hex-radius)
              points (geom/hex-points center hex-radius)]
          (dom/create-portal
           ($ :polygon.scene-object-align
             {:points (join " " (mapcat seq points))}) portal)))
      (if (and (= type :token/token) is-aligning (= base-type :hex-flat))
        (let [center (vec/nearest-hex-flat (vec/add point delta) hex-radius)
              points (geom/hex-points-flat center hex-radius)]
          (dom/create-portal
           ($ :polygon.scene-object-align
             {:points (join " " (mapcat seq points))}) portal)))
      (if (and (= type :token/token) is-aligning (not (#{:hex-pointy :hex-flat} base-type)))
        ;; Preview the rect at the point the drop will ACTUALLY produce, by
        ;; asking geom/snap-to-cell for it -- the same function
        ;; :objects/translate-many commits through. This used to round the
        ;; bounding box's own corners to grid multiples instead, which
        ;; agrees with centre-snapping only when the footprint is an odd
        ;; number of cells across. A token's footprint is `size` * 14px, so
        ;; every even size the context menu offers (10/20/30/40/50 -> 2/4/
        ;; 6/8/10 cells) previewed half a cell away from where the token
        ;; landed, and visibly jumped by 35px on release.
        (let [shift (vec/sub (geom/snap-to-cell entity delta base-type) point)
              rect (vec/add (geom/object-bounding-rect entity) shift)]
          (dom/create-portal
           ($ :rect.scene-object-align
             {:width (seg/width rect)
              :height (seg/height rect)
              :transform (.-a rect)}) portal)))
      (if (= (namespace type) "shape")
        (let [align-to (geom/object-alignment entity)
              aligned (vec/rnd (vec/add point delta) (if is-aligning align-to 1))]
          (dom/create-portal
           ($ :<>
             (if is-outline
               (let [path (geom/object-tile-path entity (vec/sub aligned point))]
                 (if (seq path)
                   ($ :polygon.scene-object-tiles
                     {:points (join " " (mapcat seq path))}))))
             (if is-aligning
               ($ :g.scene-object-ghost
                 {:transform aligned}
                 ($ :circle.scene-object-anchor {:r 3})
                 ($ :circle.scene-object-anchor-ring {:r 5})
                 ($ shape {:entity entity})))) portal))))))

(def ^:private query
  [{:root/user
    [:user/host
     :user/uuid
     [:user/bounds :default seg/zero]
     {:user/camera
      [:db/id
       :camera/selected
       [:camera/scale :default 1]
       [:camera/point :default vec/zero]
       {:camera/scene
        [[:scene/grid-align :default false]
         [:scene/grid-type :default :square]
         [:scene/show-object-outlines :default true]
         [:scene/neutral-authority? :default false]
         {:scene/game-type
          [[:game-type/enabled-elements :default #{}]]}
         {:scene/tokens
          [:db/id
           [:object/type :default :token/token]
           [:object/point :default vec/zero]
           [:object/hidden :default false]
           [:object/shared? :default false]
           [:object/layer-shift :default nil]
           [:token/label :default ""]
           [:token/flags :default #{}]
           [:token/size :default 5]
           [:token/light :default 15]
           [:token/aura-radius :default 0]
           {:token/image [:token-image/url :image/hash :image/public]}
           {:token/image-alt [:token-image/url :image/hash :image/public]}
           {:object/owner [:db/id {:player/controller [:user/uuid]}]}
           {:scene/_initiative [:db/id :initiative/turn]}]}
         {:scene/shapes
          [:db/id
           [:object/type :default :shape/circle]
           [:object/point :default vec/zero]
           [:object/locked :default false]
           [:shape/points :default [vec/zero]]
           [:shape/color :default "red"]
           [:shape/pattern :default :solid]]}
         ;; Mini-game tables. The session entity IS the scene object --
         ;; it carries :object/point/:object/scale and draws its own
         ;; owned :minigame/cards, so a running game is one movable,
         ;; scalable unit rather than N loose props.
         {:scene/minigames
          [:db/id
           :object/type
           [:object/point :default vec/zero]
           [:object/scale :default 1]
           {:minigame/cards
            [:db/id :memory/index [:memory/face-up? :default false]
             :card/rank :card/suit]}
           ;; Selection state, so a placed table can draw its own scale
           ;; handles the same way a selected prop does.
           {:camera/_selected
            [[:camera/scale :default 1]
             :camera/selected
             {:camera/scene [[:scene/grid-type :default :square]]}
             {:user/_camera [:root/_user]}]}]}
         {:scene/props
          [:db/id
           [:object/type :default :prop/prop]
           [:object/point :default vec/zero]
           [:object/scale :default 1]
           [:object/rotation :default 0]
           [:object/hidden :default false]
           [:object/shared? :default false]
           [:object/locked :default false]
           [:object/layer-shift :default nil]
           [:object/variables :default nil]
           {:prop/image
            [:image/hash
             [:image/width :default 0]
             [:image/height :default 0]
             :image/anchor]}
           {:prop/image-alt
            [:image/hash
             [:image/width :default 0]
             [:image/height :default 0]
             :image/anchor]}
           {:object/owner [:db/id {:player/controller [:user/uuid]}]}
           {:camera/_selected
            [[:camera/scale :default 1]
             [:camera/draw-mode :default :select]
             :camera/selected
             {:camera/scene [[:scene/grid-type :default :square]]}
             {:user/_camera [:root/_user]}]}]}
         {:scene/board
          [:db/id
           [:object/type :default :board/piece]
           [:object/point :default vec/zero]
           [:object/scale :default 1]
           [:object/rotation :default 0]
           [:object/rotation-mode :default :free]
           [:object/hidden :default false]
           [:object/locked :default false]
           {:board/image
            [:image/hash
             [:image/width :default 0]
             [:image/height :default 0]
             :image/anchor]}
           {:camera/_selected
            [[:camera/scale :default 1]
             [:camera/draw-mode :default :select]
             :camera/selected
             {:camera/scene [[:scene/grid-type :default :square]]}
             {:user/_camera [:root/_user]}]}]}
         {:scene/notes
          [:db/id
           [:object/type :default :note/note]
           [:object/point :default vec/zero]
           [:object/hidden :default true]
           [:object/locked :default false]
           [:note/icon :default "journal-bookmark-fill"]
           [:note/description :default ""]
           :note/label
           {:camera/_selected
            [:camera/selected
             {:user/_camera
              [{:root/_user
                [[:user/host :default true]]}]}]}]}]}]}]
    :root/session
    [{:session/conns
      [:db/ident :user/uuid :user/color :user/dragging]}]}])

(def ^:private locked-for-players-types
  "Object types that non-host players can never select or drag, regardless
   of their own :object/locked value -- notes and props are session-
   transient decoration only the host arranges, and board pieces are the
   map/board itself, set up in advance by the host. The one exception:
   an entity a non-host viewer is interactable? with (see interactable?
   -- :object/shared? true, or being the exclusive assigned controller)
   is NOT locked for them -- selecting is a prerequisite for using the
   hide/reveal control at all, so a guest who can flip a card must also
   be able to pick it up in the first place (e.g. a physical on-table
   card any player may move and flip on their turn). Board pieces/notes
   never have :object/owner/:object/shared? set by anything today, so
   this is a no-op for them -- this only actually changes behavior for
   props."
  #{:note/note :prop/prop :board/piece})

(defui objects []
  (let [dispatch (hooks/use-dispatch)
        [_ set-ready] (uix/use-state false)
        result (hooks/use-query query [:db/ident :root])
        {{bounds :user/bounds
          host :user/host
          uuid :user/uuid
          {point :camera/point
           scale :camera/scale
           selected :camera/selected
           {outline? :scene/show-object-outlines
            align? :scene/grid-align
            grid-type :scene/grid-type
            neutral? :scene/neutral-authority?
            game-type-entity :scene/game-type
            shapes :scene/shapes
            tokens :scene/tokens
            props :scene/props
            minigames :scene/minigames
            board :scene/board
            notes :scene/notes}
           :camera/scene}
          :user/camera} :root/user
         {conns :session/conns} :root/session} result
        align? (and align? (pos? (game-type/grid-count (:game-type/enabled-elements game-type-entity #{}))))
        portal (uix/use-ref)
        screen (Segment. point (vec/add point (vec/div (.-b (seg/rebase bounds)) scale)))
        selected (into #{} (map :db/id) selected)
        dragging (into {} user-drag-xf conns)
        connected-uuids (into #{} (map :user/uuid) conns)
        ;; The default authority a viewer gets over an unowned/
        ;; unassigned object -- ordinarily just "am I the host," but
        ;; suppressed on a scene in :scene/neutral-authority? mode (an
        ;; impartial-dealer scene, e.g. a Memory game in progress) so
        ;; the host doesn't automatically see hidden gameplay state
        ;; they haven't been explicitly given authority over. Used ONLY
        ;; for the two gameplay-visibility call sites below
        ;; (tokens-xf/props' resolve-hidden) -- editing-privilege checks
        ;; (Owner, Lock, Layer-shift, Remove) and board/shape/note
        ;; visibility (`visible?` below) stay on the raw `host` value,
        ;; since "the host retains control of the room" regardless of
        ;; this mode.
        default-authority (and host (not neutral?))
        bound-xf
        (comp (filter (comp selected :db/id))
              (map geom/object-bounding-rect)
              (mapcat seq))
        forward? #(= (:object/layer-shift %) :forward)
        back? #(= (:object/layer-shift %) :back)
        visible? (fn [entity] (or host (not (:object/hidden entity))))
        sorted-tokens (sort compare-tokens (sequence (tokens-xf uuid default-authority connected-uuids) tokens))
        resolved-props
        (into [] (keep #(resolve-hidden % (object-authority? uuid default-authority connected-uuids %)
                                         :prop/image :prop/image-alt))
              props)
        board-entities (sort compare-objects board)
        ;; Props/shapes/notes/tokens keep today's relative order, except a
        ;; prop can be individually shifted :forward (above the token
        ;; band) and a token can be individually shifted :back (below the
        ;; prop band) -- see :object/toggle-layer-shift. Both bands
        ;; always render above board-entities regardless (see
        ;; scene.cljs's scene-elements, which portals board-entities and
        ;; this "rest" band to two separately-orderable positions relative
        ;; to the grid, board always first/bottom). Tokens/props are
        ;; already authority-resolved above (sorted-tokens/resolved-props
        ;; -- either normal, placeholder-swapped, or dropped), so only
        ;; shapes/notes (no owner concept, still plain host-only) go
        ;; through `visible?` here.
        ;; A mini-game table sits directly above the prop band and below
        ;; shapes/notes/tokens -- it IS the table the game is played on,
        ;; so props can decorate under it while every token stays on top
        ;; of it. Only sessions that actually placed themselves on the
        ;; scene (:object/type set) render; the hand-based games have no
        ;; canvas presence at all.
        tables (filter (comp #{:minigame/table} :object/type) minigames)
        rest-entities
        (concat
         (filter back? sorted-tokens)
         (sort compare-objects (remove forward? resolved-props))
         tables
         (sort compare-objects (filter visible? shapes))
         (sort compare-objects (filter visible? notes))
         (remove back? sorted-tokens)
         (filter forward? resolved-props))
        board-entities (into [] (filter visible?) board-entities)
        entities (into [] cat [board-entities rest-entities])
        ;; A solo-selected board piece stays rendered in its own board band
        ;; (respecting :scene/grid-order-board) instead of being promoted
        ;; to the shared :selected layer like every other selected object
        ;; -- otherwise the moment a piece is selected for alignment
        ;; (dragging, scaling, rotating against the grid -- the primary
        ;; reason grid-order-board exists) it would unconditionally jump
        ;; above the grid regardless of that setting, defeating the whole
        ;; point. Multi-selections that happen to include a board piece
        ;; still promote everything to :selected as before -- a much
        ;; rarer case than solo-aligning one piece.
        solo-board?
        (fn [entity]
          (and (= (:object/type entity) :board/piece)
               (= selected #{(:db/id entity)})))
        render-entity
        (fn [entity]
          (let [{id :db/id point :object/point} entity
                lock (or (:object/locked entity)
                         (contains? dragging id)
                         (and (not host)
                              (contains? locked-for-players-types (:object/type entity))
                              (not (interactable? uuid host connected-uuids entity))))
                node (uix/create-ref)
                user (dragging id)
                rect (geom/object-bounding-rect entity)
                seen (geom/rect-intersects-rect rect screen)]
            ($ CSSTransition {:key id :nodeRef node :timeout 256}
              ($ :g.scene-object-transition {:ref node}
                (if (or (not (selected id)) (solo-board? entity))
                  ($ drag-remote-fn {:user (:user/uuid user) :point point}
                    (fn [remote]
                      ($ drag-local-fn {:id id :disabled lock}
                        (fn [^js/Object drag]
                          (let [drag-fn (and (.-listeners drag) (.-onPointerDown (.-listeners drag)))
                                drag-x (and (.-transform drag) (.-x (.-transform drag)))
                                drag-y (and (.-transform drag) (.-y (.-transform drag)))
                                local (Vec2. (or drag-x 0) (or drag-y 0))
                                delta (or remote local)]
                            ($ :g.scene-object
                              {:ref (.-setNodeRef drag)
                               :transform (vec/add point delta)
                               :tab-index (if (and (not lock) seen) 0 -1)
                               :on-pointer-down drag-fn
                               :on-double-click
                               (fn []
                                 (if (and host lock)
                                   (dispatch :objects/select (:db/id entity))))
                               :data-drag-remote (some? user)
                               :data-drag-local (.-isDragging drag)
                               :data-locked (boolean lock)
                               :data-color (:user/color user)
                               :data-type (name (keyword (namespace (:object/type entity))))
                               :data-id id}
                              ($ object {:entity entity :grid-type grid-type})
                              (if-let [portal (deref portal)]
                                ($ object-hint
                                  {:entity entity
                                   :portal portal
                                   :delta delta
                                   :is-outline outline?
                                   :is-aligned align?
                                   :grid-type grid-type})))))))))))))]

    ;; automatically re-render once the portal ref is initialized.
    (uix/use-effect
     (fn [] (set-ready true)) [])

    (use-drag-listener)
    ($ :g.scene-objects {}
      ($ :g.scene-objects-portal
        {:ref portal :tab-index -1})
      ($ hooks/use-portal {:name :board-layer}
        ($ TransitionGroup {:component nil}
          (map render-entity board-entities)))
      ($ hooks/use-portal {:name :props-layer}
        ($ TransitionGroup {:component nil}
          (map render-entity rest-entities)))
      ($ hooks/use-portal {:name :selected}
        (let [select (filter (comp selected :db/id) entities)
              bounds (transduce bound-xf geom/bounding-rect-rf entities)
              locked (or (some dragging selected)
                         (and (= (count selected) 1) (:object/locked (first select)))
                         (and (not host)
                              (some
                               (fn [entity]
                                 (and (contains? locked-for-players-types (:object/type entity))
                                      (not (interactable? uuid host connected-uuids entity))))
                               select)))]
          ($ drag-local-fn {:id "selected" :disabled locked}
            (fn [^js/Object drag]
              (let [drag-fn (and (.-listeners drag) (.-onPointerDown (.-listeners drag)))
                    drag-x (and (.-transform drag) (.-x (.-transform drag)))
                    drag-y (and (.-transform drag) (.-y (.-transform drag)))
                    local (Vec2. (or drag-x 0) (or drag-y 0))]
                ($ :g.scene-objects.scene-objects-selected
                  {:ref (.-setNodeRef drag)
                   :transform local
                   :on-pointer-down drag-fn
                   :data-drag-local (.-isDragging drag)
                   :data-locked (boolean locked)}
                  (if (> (count selected) 1)
                    ($ :rect.scene-objects-bounds
                      {:width (seg/width bounds)
                       :height (seg/height bounds)
                       :transform (.-a bounds)}))
                  ($ TransitionGroup {:component nil}
                    (for [entity entities
                          :let [{id :db/id point :object/point} entity
                                node (uix/create-ref)
                                user (dragging id)]]
                      ($ CSSTransition {:key id :nodeRef node :timeout 256}
                        ($ :g.scene-object-transition {:ref node}
                          (if (and (selected id) (not (solo-board? entity)))
                            ($ drag-remote-fn {:user (:user/uuid user) :point point}
                              (fn [remote]
                                (let [delta (or remote local)]
                                  ($ :g.scene-object
                                    {:transform (vec/add point (or remote vec/zero))
                                     :data-drag-remote (some? user)
                                     :data-drag-local (.-isDragging drag)
                                     :data-color (:user/color user)
                                     :data-id id}
                                    ($ object {:entity entity :grid-type grid-type})
                                    (if-let [portal (deref portal)]
                                      ($ object-hint
                                        {:entity entity
                                         :portal portal
                                         :delta delta
                                         :is-outline outline?
                                         :is-aligned align?
                                         :grid-type grid-type})))))))))))
                  (if (seq select)
                    (let [sz 400
                          xf (cond-> (matrix/scale matrix/identity scale)
                               (geom/iso? grid-type)
                               (matrix/multiply (geom/iso-forward-matrix grid-type)))
                          bn (xf bounds)]
                      ($ :foreignObject.context-menu-object
                        {:x (.-x (vec/shift (seg/midpoint bn) (/ sz -2)))
                         :y (.-y (.-b bn))
                         :width sz
                         :height sz
                         :transform (matrix/inverse xf)
                         :data-type (namespace (:object/type (first select)))}
                        ($ context-menu
                          {:data select
                           :host host
                           :viewer-uuid uuid
                           :connected-uuids connected-uuids
                           :default-authority default-authority})))))))))))))
