(ns ogres.app.component.panel-tokens
  (:require [goog.object :as object :refer [getValueByKeys]]
            [ogres.app.component :as component :refer [icon]]
            [ogres.app.component.panel-library-browser :refer [panel-library-browser]]
            [ogres.app.geom :as geom]
            [ogres.app.hooks :as hooks]
            [ogres.app.util :refer [separate]]
            [ogres.app.segment :as seg]
            [ogres.app.vec :as vec]
            [uix.core :as uix :refer [defui $]]
            [uix.dom :refer [create-portal]]
            [clojure.string :as str]
            ["@dnd-kit/core"
             :refer  [DndContext DragOverlay useDndMonitor useDraggable useDroppable]
             :rename {DndContext    dnd-context
                      DragOverlay   drag-overlay
                      useDndMonitor use-dnd-monitor
                      useDraggable  use-draggable
                      useDroppable  use-droppable}]))

(def ^:private query-editor
  [{:root/user [:user/host]}
   {:root/token-images
    [:image/hash
     :image/public
     :image/name
     :image/size
     :image/width
     :image/height
     :image/thumbnail-rect
     :image/thumbnail-rotation
     {:image/thumbnail
      [:image/hash :image/size]}
     :token-image/default-label
     :token-image/url]}])

(def ^:private query-actions
  [{:root/user [:user/host]}
   {:root/token-images
    [:image/hash
     :image/public
     {:image/thumbnail
      [:image/hash]}]}])

(def ^:private query-tokens
  [{:root/user [:user/host]}
   {:root/token-images
    [:image/hash
     :image/name
     :image/public
     {:image/thumbnail
      [:image/hash]}]}])

(def ^:private query-bounds
  [[:user/bounds :default seg/zero]])

(defui ^:private draggable
  [{:keys [id children]}]
  (let [opt (use-draggable #js {"id" id "data" #js {"image" "default"}})]
    (children {:options opt})))

(defui ^:private token
  [{:keys [id hash children]}]
  (let [url (hooks/use-image hash)
        opt (use-draggable #js {"id" id "data" #js {"image" hash}})]
    (children {:url url :options opt})))

(defui ^:private overlay []
  (let [[active set-active] (uix/use-state nil)]
    (use-dnd-monitor
     #js {"onDragStart" (fn [event] (set-active (.. event -active -data -current -image)))
          "onDragEnd"   (fn [_]     (set-active nil))})
    (create-portal
     ($ drag-overlay {:drop-animation nil}
       (if (= active "default")
         ($ :.token-gallery-item
           {:data-type "default"}
           ($ icon {:name "dnd"}))
         (if (some? active)
           ($ component/image {:hash active}
             (fn [url]
               ($ :.token-gallery-item {:data-type "image" :style {:background-image (str "url(" url ")")}}))))))
     (.querySelector js/document "#root"))))

(defui ^:private gallery [props]
  (let [trash-key (str (:name props) "-trash")
        trash-drag (use-droppable #js {"id" trash-key})
        [trash-active set-trash-active] (uix/use-state false)]
    (use-dnd-monitor
     #js {"onDragCancel" (fn [] (set-trash-active false))
          "onDragEnd" (fn [] (set-trash-active false))
          "onDragOver"
          (fn [event]
            (if (not= (.-id (.-active event)) "default")
              (if (and (.-over event) (= (.-id (.-over event)) trash-key))
                (set-trash-active true)
                (set-trash-active false))))})
    ($ :<>
      (for [[idx data] (sequence (map-indexed vector) (:data props))]
        (cond (map? data)
              (let [hash (:image/hash data)
                    thmb (:image/hash (:image/thumbnail data))]
                ($ token {:key hash :id hash :hash thmb}
                  (fn [{:keys [url ^js/object options]}]
                    ($ :button.token-gallery-item
                      {:ref (.-setNodeRef options)
                       :data-type "image"
                       :style {:background-image (str "url(" url ")")}
                       :on-key-down (.. options -listeners -onKeyDown)
                       :on-pointer-down (.. options -listeners -onPointerDown)
                       :aria-label (:image/name data)
                       :aria-pressed (getValueByKeys options "attributes" "aria-pressed")
                       :aria-disabled (getValueByKeys options "attributes" "aria-disabled")
                       :aria-describedby (getValueByKeys options "attributes" "aria-describedby")
                       :aria-roledescription (getValueByKeys options "attributes" "aria-roledescription")}))))
              (= data :default)
              ($ draggable {:key idx :id "default"}
                (fn [{:keys [^js/object options]}]
                  ($ :button.token-gallery-item
                    {:ref (.-setNodeRef options)
                     :data-type "default"
                     :on-key-down (.. options -listeners -onKeyDown)
                     :on-pointer-down (.. options -listeners -onPointerDown)
                     :aria-label "default"
                     :aria-pressed (getValueByKeys options "attributes" "aria-pressed")
                     :aria-disabled (getValueByKeys options "attributes" "aria-disabled")
                     :aria-describedby (getValueByKeys options "attributes" "aria-describedby")
                     :aria-roledescription (getValueByKeys options "attributes" "aria-roledescription")}
                    ($ icon {:name "dnd"}))))
              (= data :placeholder)
              ($ :.token-gallery-item {:key idx :data-type "placeholder"})))
      ($ :.token-gallery-item
        {:ref (.-setNodeRef trash-drag)
         :data-type "trash"
         :data-active trash-active}
        ($ icon {:name "trash3-fill" :size 26})))))

(defui ^:private paginated
  [{:keys [name data limit] :or {data [] limit 10}}]
  ($ component/paginated {:data data :page-size (dec limit)}
    (fn [{:keys [data pages page on-change]}]
      (let [data (->> (repeat :placeholder) (concat data) (take (dec limit)))]
        ($ :<>
          ($ gallery {:name name :data data})
          (if (> pages 1)
            ($ component/pagination
              {:name name
               :label "Token image pages"
               :pages pages
               :value page
               :on-change on-change})))))))

(defui tokens []
  (let [result (hooks/use-query query-tokens [:db/ident :root])
        {data :root/token-images
         {host :user/host} :root/user} result
        [active set-active] (uix/use-state nil)
        [pub prv] (separate :image/public data)
        data-pub  (into [:default] (reverse pub))
        data-prv  (vec (reverse prv))
        drop-pub  (use-droppable #js {"id" "scope-pub"})
        drop-prv  (use-droppable #js {"id" "scope-prv"})]
    (use-dnd-monitor
     #js {"onDragCancel" (fn [] (set-active nil))
          "onDragEnd" (fn [] (set-active nil))
          "onDragOver"
          (fn [event]
            (if (not= (.-id (.-active event)) "default")
              (let [dx (.-x (.-delta event))
                    dy (.-y (.-delta event))]
                (if (or (not= dx 0) (not= dy 0))
                  (if (.-over event)
                    (set-active (.-id (.-over event)))
                    (set-active nil))))))})
    ($ :.form-tokens
      (if host
        ($ :<>
          ($ :header ($ :h2 "Tokens"))
          ($ :fieldset.fieldset.token-gallery
            {:ref (.-setNodeRef drop-pub)
             :data-type "host"
             :data-scope "public"
             :data-active (= active "scope-pub")}
            ($ :legend "Public")
            ($ paginated {:name "tokens-public" :data data-pub :limit 10}))
          ($ :fieldset.fieldset.token-gallery
            {:ref (.-setNodeRef drop-prv)
             :data-type "host"
             :data-scope "private"
             :data-active (= active "scope-prv")}
            ($ :legend "Private")
            ($ paginated {:name "tokens-private" :data data-prv :limit 20}))
          ($ :.form-notice
            "Upload images from your computer and pull them onto the scene as
             tokens. Moving tokens to the public section will make them available
             for other players in an online game."))
        ($ :<>
          ($ :header ($ :h2 "Tokens"))
          ($ :fieldset.fieldset.token-gallery
            {:ref (.-setNodeRef drop-pub)
             :data-type "conn"
             :data-scope "public"
             :data-active (= active "scope-pub")}
            ($ :legend "Tokens")
            ($ paginated {:name "tokens-public" :data data-pub :limit 30})))))))

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        results  (hooks/use-query query-bounds [:db/ident :user])
        {bounds :user/bounds} results
        on-create
        (uix/use-callback
         (fn [hash element delta]
           (let [token (seg/DOMRect-> (.getBoundingClientRect element))
                 point (-> (vec/add (.-a token) delta)
                           (vec/add (vec/sub (seg/midpoint token) (.-a token)))
                           (vec/sub (.-a bounds)))]
             (if (geom/point-within-rect? point bounds)
               (if (= hash "default")
                 (dispatch :token/create point nil)
                 (dispatch :token/create point hash)))))
         [dispatch bounds])]
    ($ dnd-context
      #js {"onDragEnd"
           (uix/use-callback
            (fn [event]
              (let [drag (getValueByKeys event #js ["active" "id"])
                    drop (getValueByKeys event #js ["over" "id"])
                    nail (getValueByKeys event #js ["active" "data" "current" "image"])]
                (if (and (some? drop) (not= drag "default"))
                  (cond
                    (= drop "scope-pub")
                    (dispatch :token-images/change-scope drag true)
                    (= drop "scope-prv")
                    (dispatch :token-images/change-scope drag false)
                    (str/includes? drop "trash")
                    (dispatch :token-images/remove drag nail))
                  (let [target (.. event -activatorEvent -target)
                        delta  (.-delta event)]
                    (on-create drag target delta)))))
            [dispatch on-create])}
      ($ tokens)
      ($ overlay))))

(defn ^:private object-fit-scale
  "Returns the amount the image given by its natural dimensions {Aw Ah} has
   been scaled to fit into the container given by {Bw Bh} as a number in
   the domain (0, 1]; 1 being that the image dimensions have not been
   scaled at all."
  [aw ah bw bh]
  (min (/ bh ah) (/ bw aw) 1))

(defn ^:private default-region [x y]
  (if (<= x y)
    [0 0 x x]
    [(/ (- x y) 2) 0
     (/ (+ x y) 2) y]))

(defn ^:private dnd-scale-fn
  "Returns a modifier function which scales the transform delta to
   the given ratio."
  [scale]
  (fn [data]
    (let [dx (.. data -transform -x)
          dy (.. data -transform -y)]
      (js/Object.assign
       #js {} (.-transform data)
       #js {"x" (/ dx scale)
            "y" (/ dy scale)}))))

(defn ^:private dnd-integer-fn
  "Returns a new transform delta whose terms are coerced to integers."
  [data]
  (let [dx (.. data -transform -x)
        dy (.. data -transform -y)]
    (js/Object.assign
     #js {} (.-transform data)
     #js {"x" (int dx)
          "y" (int dy)})))

(defn ^:private dnd-clamp-fn
  "Returns a modifier function which clamps the transform delta to
   the dimensions of the image given as {Wd Ht}."
  [[ax ay bx by] wd ht]
  (fn [data]
    (if (some? (.-active data))
      (let [id (keyword (.. data -active -id))
            dx (.. data -transform -x)
            dy (.. data -transform -y)]
        (js/Object.assign
         #js {} (.-transform data)
         #js {"x" (case id
                    :rg (max (- ax)           (min dx (- wd bx)))
                    :nw (max (- ay) (- ax)    (min dx (- bx ax)))
                    :ne (max (- ax bx)        (min dx (- wd bx) ay))
                    :se (max (- ax bx)        (min dx (- wd bx) (- ht by)))
                    :sw (max (- by ht) (- ax) (min dx (- bx ax))))
              "y" (case id
                    :rg (max (- ay)           (min dy (- ht by)))
                    :nw (max (- ay) (- ax)    (min dy (- by ay)))
                    :ne (max (- ay)           (min dy (- by ay)))
                    :se (max (- ay by)        (min dy (- ht by) (- wd bx)))
                    :sw (max (- ay by)        (min dy (- ht by))))}))
      (.-transform data))))

(defui ^:private anchor [{:keys [id]}]
  (let [drag (use-draggable #js {"id" (name id)})]
    ($ :.token-editor-region-anchor
      {:ref (.-setNodeRef drag)
       :class (str "token-editor-region-anchor-" (name id))
       :on-pointer-down (.. drag -listeners -onPointerDown)})))

(defn ^:private rotate-rect
  "Maps a crop rectangle from a `w` x `h` frame into the frame produced by
   turning that image one quarter turn, so the selection keeps covering the
   same pixels instead of jumping somewhere else when the image turns."
  [[ax ay bx by] w h clockwise?]
  (if clockwise?
    [(- h by) ax (- h ay) bx]
    [ay (- w bx) by (- w ax)]))

(defn ^:private use-rotated-url
  "Returns an object URL for `url` turned clockwise by `degrees`. The editor
   displays a genuinely rotated image rather than a CSS-transformed one so
   that every measurement taken off it -- the crop rectangle, its clamp, the
   object-fit scale -- is in the same frame the user sees."
  [url degrees]
  (let [[out set-out] (uix/use-state nil)]
    (uix/use-effect
     (fn []
       (if (or (nil? url) (zero? (mod degrees 360)))
         (do (set-out url) js/undefined)
         (let [image (js/Image.)
               made  (atom nil)]
           (set! (.-onload image)
                 (fn []
                   (let [deg (mod degrees 360)
                         sw  (.-naturalWidth image)
                         sh  (.-naturalHeight image)
                         qtr (or (= deg 90) (= deg 270))
                         cw  (if qtr sh sw)
                         ch  (if qtr sw sh)
                         cvs (js/document.createElement "canvas")
                         ctx (do (set! (.-width cvs) cw)
                                 (set! (.-height cvs) ch)
                                 (.getContext cvs "2d"))]
                     (.translate ctx (/ cw 2) (/ ch 2))
                     (.rotate ctx (* deg (/ js/Math.PI 180)))
                     (.drawImage ctx image (- (/ sw 2)) (- (/ sh 2)))
                     (.toBlob cvs (fn [blob]
                                    (let [next (js/URL.createObjectURL blob)]
                                      (reset! made next)
                                      (set-out next)))))))
           (set! (.-src image) url)
           (fn [] (if-let [prev (deref made)] (js/URL.revokeObjectURL prev))))))
     [url degrees])
    out))

(defui ^:private region [{:keys [children x y width height]}]
  (let [drag (use-draggable #js {"id" "rg"})]
    ($ :.token-editor-region
      ($ :.token-editor-region-drag
        {:ref (.-setNodeRef drag)
         :style {:left x :top y :width width :height height}
         :on-pointer-down (.. drag -listeners -onPointerDown)}
        children))))

(defui
  ^{:doc "The token crop/rotate editor's actual workspace -- an image,
          a draggable/resizable crop region over it, and rotate-left/
          rotate-right/apply controls. Public (not ^:private) so
          panel_library_browser.cljs can reuse it directly for an
          archived (not necessarily placed in any gallery) token --
          it's keyed entirely off `(:image props)` (a pulled image
          entity carrying at least :image/hash/:width/:height, plus
          :image/thumbnail-rect/:thumbnail-rotation when already
          calibrated), never off a live placed object, so it works the
          same whether or not the token is currently active anywhere.
          `token-editor` below is just this plus a gallery-picker
          wrapper for the Tokens panel's own multi-select workflow."}
  editor [props]
  (let [{{hash   :image/hash
          width  :image/width
          height :image/height} :image
         on-change :on-change} props
        element (uix/use-ref nil)
        ;; Quarter turns. The image the user is looking at is really rotated
        ;; rather than merely transformed in CSS, so the crop rectangle, its
        ;; clamp, and the object-fit scale are all expressed in the frame on
        ;; screen -- what you crop is what gets saved.
        [rotation set-rotation]
        (uix/use-state (or (:image/thumbnail-rotation (:image props)) 0))
        quarter? (or (= rotation 90) (= rotation 270))
        rot-wdth (if quarter? height width)
        rot-hght (if quarter? width height)
        obj-url (use-rotated-url (hooks/use-image hash) rotation)
        initial (or (:image/thumbnail-rect (:image props))
                    (default-region rot-wdth rot-hght))
        [delta set-delta] (uix/use-state [0 0 0 0])
        [bound set-bound] (uix/use-state initial)
        [scale set-scale] (uix/use-state nil)
        scale-fn (uix/use-memo #(dnd-scale-fn scale) [scale])
        clamp-fn (uix/use-memo #(dnd-clamp-fn bound rot-wdth rot-hght) [bound rot-wdth rot-hght])
        on-drag-move
        (uix/use-callback
         (fn [data]
           (let [id (.. data -active -id)
                 dx (.. data -delta -x)
                 dy (.. data -delta -y)
                 si (js/Math.sign dx)
                 mx (abs dx)]
             (set-delta
              (fn [[ax ay bx by]]
                (case (keyword id)
                  :rg [dx dy dx dy]
                  :nw [(min dx dy) (min dx dy) bx by]
                  :ne [ax (- (* si mx)) (* si mx) by]
                  :se [ax ay (max dx dy) (max dx dy)]
                  :sw [(* si mx) ay bx (- (* si mx))]))))) [])
        on-drag-stop
        (uix/use-callback
         (fn [data]
           (let [id (.. data -active -id)
                 dx (.. data -delta -x)
                 dy (.. data -delta -y)
                 si (js/Math.sign dx)
                 mx (abs dx)]
             (set-delta [0 0 0 0])
             (set-bound
              (fn [[ax ay bx by]]
                (case (keyword id)
                  :rg [(+ ax dx) (+ ay dy) (+ bx dx) (+ by dy)]
                  :nw [(+ ax (min dx dy)) (+ ay (min dx dy)) bx by]
                  :ne [ax (+ ay (- (* si mx))) (+ bx (* si mx)) by]
                  :se [ax ay (+ bx (max dx dy)) (+ by (max dx dy))]
                  :sw [(+ ax (* si mx)) ay bx (+ by (- (* si mx)))]))))) [])
        on-resize-image
        (uix/use-callback
         (fn [[entry]]
           (let [con-rect (.-contentRect entry)
                 con-wdth (js/Math.round (.-width con-rect))
                 con-hght (js/Math.round (.-height con-rect))]
             (set-scale (object-fit-scale rot-wdth rot-hght con-wdth con-hght))))
         [rot-wdth rot-hght])]

    ;; Observe changes to the size of the token image in order to recalculate
    ;; the scale of the image relative to its natural dimensions.
    (uix/use-effect
     (fn []
       (let [resizes (js/ResizeObserver. on-resize-image)]
         (if-let [node (deref element)] (.observe resizes node))
         (fn [] (.disconnect resizes)))) [on-resize-image])

    ($ dnd-context
      #js {"modifiers"  #js [scale-fn dnd-integer-fn clamp-fn]
           "onDragMove" on-drag-move
           "onDragEnd"  on-drag-stop}
      (let [[ax ay bx by] bound
            [cx cy dx dy] delta]
        ($ :form
          {:on-submit
           (fn [event]
             (.preventDefault event)
             (on-change hash bound rotation))}
          ($ :img.token-editor-image
            {:ref element
             :src obj-url
             :width rot-wdth
             :height rot-hght
             :data-loaded (some? obj-url)})
          ($ region
            {:x      (* (- (+ ax cx) (/ rot-wdth 2)) scale)
             :y      (* (- (+ ay cy) (/ rot-hght 2)) scale)
             :width  (* (+ (- bx ax cx) dx) scale)
             :height (* (+ (- by ay cy) dy) scale)}
            ($ anchor {:key :nw :id :nw})
            ($ anchor {:key :ne :id :ne})
            ($ anchor {:key :se :id :se})
            ($ anchor {:key :sw :id :sw}))
          ($ :.token-editor-actions
            ($ :button.button.button-neutral
              {:type "button"
               :aria-label "Rotate left"
               :data-tooltip "Rotate left"
               :on-click
               (fn []
                 (set-bound (fn [rect] (rotate-rect rect rot-wdth rot-hght false)))
                 (set-rotation #(mod (- % 90) 360)))}
              ($ icon {:name "arrow-counterclockwise" :size 16}))
            ($ :button.button.button-neutral.token-editor-rotate-cw
              {:type "button"
               :aria-label "Rotate right"
               :data-tooltip "Rotate right"
               :on-click
               (fn []
                 (set-bound (fn [rect] (rotate-rect rect rot-wdth rot-hght true)))
                 (set-rotation #(mod (+ % 90) 360)))}
              ;; No clockwise glyph exists; mirroring the counterclockwise
              ;; one beats shipping a near-duplicate symbol for it.
              ($ icon {:name "arrow-counterclockwise" :size 16}))
            ($ :button.token-editor-button
              ;; Enabled by a rotation as well as a crop -- rotating
              ;; without re-cropping is a perfectly ordinary edit.
              {:type "submit" :disabled (and (= initial bound) (zero? rotation))}
              ($ icon {:name "crop" :size 16})
              "Apply")))))))

(defui ^:private token-editor [props]
  (let [dispatch (hooks/use-dispatch)
        publish (hooks/use-publish)
        {user :root/user images :root/token-images} (hooks/use-query query-editor [:db/ident :root])
        [selected set-selected] (uix/use-state nil)
        data  (reverse (filter (comp (if (:user/host user) any? true?) :image/public) images))
        token (first (filter (comp #{selected} :image/hash) data))]
    ($ component/fullscreen-dialog
      {:on-close (:on-close props)}
      ($ :.token-editor
        (if token
          ($ :.token-editor-workspace
            ($ editor
              {:key (:image/hash token)
               :image token
               :on-change
               (fn [hash bounds rotation]
                 (if (:user/host user)
                   (publish :image/change-thumbnail hash bounds rotation)
                   (publish :image/change-thumbnail-request hash bounds rotation)))}))
          ($ :.token-editor-placeholder
            ($ icon {:name "crop" :size 64})
            "Select an image to crop and resize." ($ :br)
            "The original image will always be preserved."))
        ($ :.token-editor-gallery
          ($ component/paginated {:data data :page-size 20}
            (fn [{:keys [data pages page on-change]}]
              ($ :.token-editor-gallery-paginated
                ($ :.token-editor-gallery-thumbnails
                  (for [{{hash :image/hash} :image/thumbnail key :image/hash name :image/name} data]
                    ($ component/image {:key key :hash hash}
                      (fn [url]
                        ($ :label.token-editor-gallery-thumbnail
                          {:style {:background-image (str "url(" url ")")} :aria-label name}
                          ($ :input
                            {:type "radio"
                             :name "token-editor-image"
                             :checked (= selected key)
                             :value key
                             :on-change
                             (fn []
                               (set-selected key))}))))))
                ($ component/pagination
                  {:name "token-editor-gallery"
                   :label "Token image pages"
                   :pages pages
                   :value page
                   :on-change on-change
                   :class-name "dark"}))))
          (if token
            ($ :form.token-editor-options
              {:key selected
               :on-submit
               (fn [event]
                 (let [data (js/FormData. (.-target event))]
                   (.preventDefault event)
                   (dispatch
                    :token-images/change-details
                    selected
                    (.get data "label")
                    (.get data "url"))
                   ((:on-close props))))
               :on-blur
               (fn [event]
                 (let [value (.. event -target -value)]
                   (condp = (.. event -target -name)
                     "label" (dispatch :token-images/change-default-label selected value)
                     "url"   (dispatch :token-images/change-url selected value))))}
              ($ :fieldset.fieldset
                ($ :legend "Default Label")
                ($ :input.text.text-ghost
                  {:type "text"
                   :name "label"
                   :default-value (:token-image/default-label token)
                   :spell-check false
                   :auto-complete "off"}))
              ($ :fieldset.fieldset
                ($ :legend "URL")
                ($ :input.text.text-ghost
                  {:type "text"
                   :name "url"
                   :default-value (:token-image/url token)
                   :spell-check false
                   :auto-complete "off"}))
              ($ :button {:type "submit" :hidden true})))
          ($ :button.token-editor-button
            {:on-click (:on-close props)}
            "Exit"))))))

(defn ^:private render-token-editor
  "Passed to panel-library-browser as its `render-editor` prop -- see
   that namespace's docstring for why this indirection exists (avoids
   a circular require, since panel_tokens.cljs already requires
   panel_library_browser.cljs for its own 'Browse Library' button).
   `entry`'s own hash is enough to crop/rotate it regardless of
   whether it's currently placed anywhere -- the same publish/dispatch
   path token-editor's own per-token save uses."
  [publish host entry on-done]
  ($ editor
    {:image entry
     :on-change
     (fn [hash bounds rotation]
       (if host
         (publish :image/change-thumbnail hash bounds rotation)
         (publish :image/change-thumbnail-request hash bounds rotation))
       (on-done))}))

(defui ^:memo actions []
  (let [[editing set-editing] (uix/use-state false)
        [browsing? set-browsing!] (uix/use-state false)
        dispatch (hooks/use-dispatch)
        publish  (hooks/use-publish)
        result   (hooks/use-query query-actions [:db/ident :root])
        {{host :user/host} :root/user
         images :root/token-images} result]
    ($ :<>
      (if editing
        ($ token-editor {:on-close #(set-editing false)}))
      (if browsing?
        ($ panel-library-browser
          {:gallery :token
           :on-close #(set-browsing! false)
           :render-editor (partial render-token-editor publish host)}))
      ($ :button.button.button-neutral
        {:type "button" :on-click #(set-browsing! true)}
        ($ icon {:name "images" :size 16}) "Browse Library")
      ($ :button.button.button-neutral
        {:type "button"
         :title "Crop"
         :disabled (not (seq (filter (comp (if host any? true?) :image/public) images)))
         :on-click (partial set-editing not)}
        ($ icon {:name "crop" :size 18})
        "Edit")
      ($ :button.button.button-danger
        {:type "button"
         :title "Remove all from this gallery -- stays in the library archive"
         :aria-label "Remove all tokens"
         :disabled (or (not host) (not (seq images)))
         :on-click (fn [] (dispatch :token-images/remove-all))}
        ($ icon {:name "trash3-fill" :size 16})))))
