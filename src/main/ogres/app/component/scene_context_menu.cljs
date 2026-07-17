(ns ogres.app.component.scene-context-menu
  (:require [ogres.app.component :refer [icon]]
            [ogres.app.component.scene-pattern :refer [pattern]]
            [ogres.app.game-type :as game-type]
            [ogres.app.geom :as geom]
            [ogres.app.hooks :as hooks]
            [ogres.app.util :as util]
            [uix.core :as uix :refer [defui $]]))

(defn ^:private token-size [x]
  (cond (<= x 3)  "Tiny"
        (<  x 5)  "Small"
        (<= x 5)  "Medium"
        (<= x 10) "Large"
        (<= x 15) "Huge"
        (>  x 15) "Gargantuan"
        :else     "Unknown"))

(def ^:private enabled-elements-query
  [{:user/camera
    [{:camera/scene
      [{:scene/game-type
        [[:game-type/enabled-elements :default #{}]]}]}]}])

(defn ^:private use-enabled-elements []
  (let [result (hooks/use-query enabled-elements-query)]
    (-> result :user/camera :camera/scene :scene/game-type :game-type/enabled-elements)))

(def ^:private shape-colors
  ["red"   "orange"  "amber"  "yellow" "lime"
   "green" "emerald" "teal"   "cyan"   "sky"
   "blue"  "indigo"  "violet" "purple" "fuchsia"])

(def ^:private shape-patterns
  [{:value :solid   :label "Fill"}
   {:value :empty   :label "Empty"}
   {:value :lines   :label "Lines"}
   {:value :crosses :label "Crosses"}
   {:value :caps    :label "Caps"}])

(defn ^:private prevent-default [event]
  (.preventDefault event))

(defn ^:private stop-propagation [event]
  (.stopPropagation event))

(defui ^:private action-hide
  [{:keys [value disabled on-change]
    :or   {value false disabled false on-change prevent-default}}]
  ($ :label.context-menu-action
    {:data-tooltip (if value "Reveal" "Hide")}
    ($ :input
      {:type "checkbox"
       :name "hidden"
       :checked value
       :disabled disabled
       :aria-disabled disabled
       :on-change
       (fn [event]
         (on-change (.-checked (.-target event))))})
    ($ icon {:name (if value "eye-slash-fill" "eye-fill")})))

(defui ^:private action-lock
  [{:keys [value disabled on-change]
    :or   {value false disabled false on-change prevent-default}}]
  ($ :label.context-menu-action
    {:data-tooltip (if value "Unlock" "Lock")}
    ($ :input
      {:type "checkbox"
       :name "hidden"
       :checked value
       :disabled disabled
       :aria-disabled disabled
       :on-change
       (fn [event]
         (on-change (.-checked (.-target event))))})
    ($ icon {:name (if value "lock" "unlock")})))

(defui ^:private action-layer-shift
  "A checkbox-style toggle action, styled and behaving just like
   action-hide/action-lock above, for a single :object/layer-shift value
   (:forward for props, :back for tokens) -- see
   :objects/toggle-layer-shift-selected."
  [{:keys [value disabled tooltip icon-name on-change]
    :or   {value false disabled false on-change prevent-default}}]
  ($ :label.context-menu-action
    {:data-tooltip tooltip}
    ($ :input
      {:type "checkbox"
       :name "layer-shift"
       :checked value
       :disabled disabled
       :aria-disabled disabled
       :on-change
       (fn [event]
         (on-change (.-checked (.-target event))))})
    ($ icon {:name icon-name})))

(defui ^:private action-remove
  [{:keys [disabled on-click]
    :or   {disabled false on-click prevent-default}}]
  ($ :button
    {:type "button"
     :disabled disabled
     :data-tooltip "Remove"
     :style {:margin-left "auto"}
     :on-click (fn [] (on-click))}
    ($ icon {:name "trash3-fill"})))

(defui ^:private context-menu-fn
  [{:keys [render-toolbar render-aside children]
    :or   {render-toolbar (constantly nil)
           render-aside   (constantly nil)}}]
  (let [[selected set-selected] (uix/use-state nil)
        props {:selected  selected
               :on-change (fn [form]
                            (if (= selected form)
                              (set-selected nil)
                              (set-selected form)))}]
    ($ :.context-menu {:on-pointer-down stop-propagation}
      ($ :.context-menu-main {:data-expanded (some? selected)}
        ($ :.context-menu-toolbar
          (render-toolbar props))
        (if selected
          ($ :.context-menu-form
            {:class (str "context-menu-form-" (name selected))}
            (children props))))
      ($ :.context-menu-aside
        ($ :.context-menu-toolbar
          (render-aside props))))))

(defui ^:private token-form-label
  [{:keys [values on-change on-close]
    :or   {values    (constantly (list))
           on-change identity
           on-close  identity}}]
  (let [[dirty set-dirty] (uix/use-state false)
        input (uix/use-ref)
        label (let [vs (values :token/label)]
                (if (= (count vs) 1) (first vs) ""))]
    (uix/use-effect
     (fn [] (.select @input)) [])
    ($ :form
      {:on-submit
       (fn [event]
         (.preventDefault event)
         (let [value (.. event -target -elements -label -value)]
           (on-change :token/change-label value)
           (on-close)))
       :on-blur
       (fn [event]
         (when dirty
           (on-change :token/change-label (.-value (.-target event))))
         (on-close))}
      ($ :input.text.text-ghost
        {:type "text"
         :name "label"
         :placeholder "Change token label"
         :default-value label
         :ref input
         :auto-focus true
         :on-change
         (fn []
           (set-dirty true))}))))

(defui ^:private token-form-details
  [{:keys [on-change values enabled-elements]
    :or   {values (constantly (list)) on-change identity enabled-elements #{}}}]
  (let [value-fn (fn [attr] (first (into (sorted-set-by >) (values attr))))]
    ($ :<>
      (if (contains? enabled-elements :unit/size)
        (let [value (value-fn :token/size)]
          ($ :<>
            ($ :label "Size")
            ($ :button
              {:type "button"
               :auto-focus true
               :on-click #(on-change :token/change-size (max (- value 5) 5))
               :aria-label "Decrease token size by 5 feet"}
              "-")
            ($ :data {:value value}
              (str value  "ft. " (token-size value)))
            ($ :button
              {:type "button"
               :on-click #(on-change :token/change-size (min (+ value 5) 50))
               :aria-label "Increase token size by 5 feet"} "+"))))
      (if (contains? enabled-elements :unit/light)
        (let [value (value-fn :token/light)]
          ($ :<>
            ($ :label "Light")
            ($ :button
              {:type "button"
               :on-click #(on-change :token/change-light (max (- value 5) 0))
               :aria-label "Decrease light radius by 5 feet"}
              "-")
            ($ :data {:value value}
              (if (> value 0) (str value "ft. radius") "None"))
            ($ :button
              {:type "button"
               :on-click #(on-change :token/change-light (min (+ value 5) 120))
               :aria-label "Increase light radius by 5 feet"}
              "+"))))
      (if (contains? enabled-elements :unit/aura)
        (let [value (value-fn :token/aura-radius)]
          ($ :<>
            ($ :label "Aura")
            ($ :button
              {:type "button"
               :on-click #(on-change :token/change-aura (max (- value 5) 0))
               :aria-label "Decrease aura size by 5 feet"}
              "-")
            ($ :data {:value value}
              (if (> value 0) (str value "ft. radius") "None"))
            ($ :button
              {:type "button"
               :on-click #(on-change :token/change-aura (min (+ value 5) 120))
               :aria-label "Increase aura size by 5 feet"}
              "+")))))))

(defui ^:private context-menu-token [props]
  (let [dispatch (hooks/use-dispatch)
        data     (:data props)
        idxs     (into [] (map :db/id) data)
        enabled  (use-enabled-elements)
        details? (some enabled #{:unit/size :unit/light :unit/aura})
        ;; Any enabled element that declares a :token-panel gets its own
        ;; toolbar tab + form body here -- this file never names a specific
        ;; game or mechanic, it only looks for the presence of that key.
        panel-elements (filter (comp :token-panel val)
                                (select-keys game-type/elements enabled))]
    ($ context-menu-fn
      {:render-toolbar
       (fn [{:keys [selected on-change]}]
         ($ :<>
           (for [[form icon-name tooltip]
                 (into (cond-> [[:label "fonts" "Label"]]
                         details? (conj [:details "sliders" "Options"]))
                       (map (fn [[id element]]
                              [id
                               (get-in element [:token-panel :icon])
                               (get-in element [:token-panel :tooltip])]))
                       panel-elements)]
             ($ :button
               {:key form
                :type "button"
                :data-selected (= selected form)
                :data-tooltip tooltip
                :on-click #(on-change form)}
               ($ icon {:name icon-name})))
           (if (contains? enabled :unit/initiative)
             (let [on (every? (comp vector? :scene/_initiative) data)]
               ($ :button
                 {:type "button"
                  :data-selected on
                  :data-tooltip "Turn Order"
                  :on-click #(dispatch :initiative/toggle idxs (not on))}
                 ($ icon {:name "hourglass-split"}))))
           (if (contains? enabled :unit/player)
             (let [on (every? (comp boolean :player :token/flags) data)]
               ($ :button
                 {:type "button"
                  :data-tooltip "Player"
                  :data-selected on
                  :on-click #(dispatch :token/change-flag idxs :player (not on))}
                 ($ icon {:name "people-fill"}))))
           (if (contains? enabled :unit/dead)
             (let [on (every? (comp boolean :dead :token/flags) data)]
               ($ :button
                 {:type "button"
                  :data-tooltip "Dead"
                  :data-selected on
                  :on-click #(dispatch :token/change-dead idxs (not on))}
                 ($ icon {:name "skull"}))))
           (let [xfr (comp :token-image/url :token/image)
                 url (js/URL.parse (xfr (first data)))]
             (if (and (some? url)
                      (util/uniform-by xfr data)
                      (or (:host props)
                          (every? (comp :image/public :token/image) data)))
               ($ :a {:href (.-href url) :target "_blank" :data-tooltip "Open link"}
                 ($ icon {:name "box-arrow-up-right"}))))))
       :render-aside
       (fn []
         ($ :<>
           ($ action-hide
             {:value (every? :object/hidden data)
              :disabled (not (:host props))
              :on-change
              (fn []
                (dispatch :objects/toggle-hidden-selected))})
           ($ action-layer-shift
             {:value (every? (comp #{:back} :object/layer-shift) data)
              :disabled (not (:host props))
              :tooltip "Send back below props"
              :icon-name "arrow-down-short"
              :on-change
              (fn []
                (dispatch :objects/toggle-layer-shift-selected :back))})
           ($ action-remove
             {:on-click
              (fn []
                (dispatch :objects/remove-selected))})))}
      (fn [{:keys [selected on-change]}]
        (let [props {:on-close  #(on-change nil)
                     :on-change #(apply dispatch %1 idxs %&)
                     :enabled-elements enabled
                     :values    (fn vs
                                  ([f] (vs f #{}))
                                  ([f init] (into init (map f) data)))}]
          (case selected
            :label   ($ token-form-label props)
            :details ($ token-form-details props)
            (if-let [element (get game-type/elements selected)]
              ((get-in element [:token-panel :render]) props))))))))

(defui ^:private shape-form-style
  [{:keys [on-change values]}]
  ($ :.context-menu-form-styles
    (for [{:keys [value label]} shape-patterns]
      (let [id (str "template-pattern-" (name value))]
        ($ :label {:key value :aria-label label}
          ($ :input
            {:type "radio"
             :name "shape-pattern"
             :value value
             :checked (= value (first (values :shape/pattern)))
             :on-change
             (fn [event]
               (let [value (.. event -target -value)]
                 (on-change :objects/update :shape/pattern (keyword value))))})
          ($ :svg
            ($ :defs ($ pattern {:id id :name value}))
            ($ :rect {:width "100%" :height "100%" :fill (str "url(#" id ")")})))))
    (for [value shape-colors]
      ($ :label {:key value :aria-label value :data-color value}
        ($ :input
          {:type "radio"
           :name "shape-color"
           :value value
           :checked (= value (first (values :shape/color)))
           :on-change
           (fn [event]
             (on-change :objects/update :shape/color (.. event -target -value)))})))))

(defui ^:private context-menu-shape [props]
  (let [dispatch (hooks/use-dispatch)
        data     (:data props)]
    ($ context-menu-fn
      {:render-toolbar
       (fn [{:keys [selected on-change]}]
         ($ :button
           {:type "button"
            :data-selected (= selected :style)
            :data-tooltip "Style"
            :on-click #(on-change :style)}
           ($ icon {:name "palette-fill"})))
       :render-aside
       (fn []
         ($ :<>
           ($ action-lock
             {:value (every? :object/locked data)
              :disabled (not (:host props))
              :on-change
              (fn []
                (dispatch :objects/toggle-locked-selected))})
           ($ action-remove
             {:on-click
              (fn []
                (dispatch :objects/remove-selected))})))}
      (fn [{:keys [selected]}]
        (if (= selected :style)
          ($ shape-form-style
            {:values
             (fn vs
               ([f] (vs f #{}))
               ([f init] (into init (map f) data)))
             :on-change
             (fn [event & args]
               (apply dispatch event (map :db/id data) args))}))))))

(defui context-menu-prop [props]
  (let [dispatch (hooks/use-dispatch)
        data     (:data props)
        entity   (first data)
        id       (:db/id entity)
        camera   (first (:camera/_selected entity))
        anchor-editing? (= (:camera/draw-mode camera) :object-anchor)]
    (if anchor-editing?
      ;; While actively dragging the grid-anchor marker (see
      ;; object-prop-edit in scene_objects.cljs), the confirm/cancel
      ;; controls render right next to the marker on the canvas instead
      ;; of here, so this menu steps out of the way entirely rather than
      ;; showing two separate floating controls over the same piece.
      nil
      ($ context-menu-fn
        {:render-toolbar
         (fn []
           ($ :<>
             ($ :button
               {:type "button"
                :data-tooltip "Reset size/rotation"
                :on-click
                (fn []
                  (dispatch :objects/reset-transform-selected))}
               ($ icon {:name "arrows-angle-expand"}))
             ($ :button
               {:type "button"
                :data-tooltip "Save scale as default"
                :on-click
                (fn []
                  ;; Calibration reads a single entity's own scale/image, so
                  ;; a multi-selection just uses the first -- this mirrors
                  ;; the predecessor project's "Save scale as default" button
                  ;; being a single-piece action.
                  (dispatch :image/set-cell-scale id))}
               ($ icon {:name "floppy"}))
             ($ :button
               {:type "button"
                :data-tooltip "Save rotation as default"
                :on-click
                (fn []
                  ;; Same single-piece convention as "Save scale as
                  ;; default" above -- a multi-selection just uses the
                  ;; first entity.
                  (dispatch :image/set-rotation id))}
               ($ icon {:name "arrow-counterclockwise-lock"}))
             ($ :button
               {:type "button"
                :data-tooltip "Set grid anchor"
                :on-click
                (fn []
                  (dispatch :camera/change-mode :object-anchor))}
               ($ icon {:name "anchor"}))))
         :render-aside
         (fn []
           ($ :<>
             ($ action-hide
               {:value (every? :object/hidden data)
                :disabled (not (:host props))
                :on-change
                (fn []
                  (dispatch :objects/toggle-hidden-selected))})
             ($ action-lock
               {:value (every? :object/locked data)
                :disabled (not (:host props))
                :on-change
                (fn []
                  (dispatch :objects/toggle-locked-selected))})
             ($ action-layer-shift
               {:value (every? (comp #{:forward} :object/layer-shift) data)
                :disabled (not (:host props))
                :tooltip "Bring forward above tokens"
                :icon-name "arrow-up-short"
                :on-change
                (fn []
                  (dispatch :objects/toggle-layer-shift-selected :forward))})
             ($ action-remove
               {:on-click
                (fn []
                  (dispatch :objects/remove-selected))})))}
        (fn [{:keys []}])))))

(def ^:private options-board-rotation-hex
  [["Free" :free] ["15°" 15] ["30°" 30] ["60°" 60] ["90°" 90]])

(def ^:private options-board-rotation-square
  [["Free" :free] ["45°" 45] ["90°" 90]])

(defn ^:private board-rotation-options
  "Board piece rotation-snap choices, filtered by grid family -- same
   split as the panel's own rotation-mode options (panel_scene.cljs),
   duplicated here rather than shared since panels and this canvas-side
   menu are different layers of the app."
  [grid-type]
  (if (= (geom/base-grid-type grid-type) :square)
    options-board-rotation-square
    options-board-rotation-hex))

(defui context-menu-board [props]
  (let [dispatch (hooks/use-dispatch)
        data     (:data props)
        entity   (first data)
        id       (:db/id entity)
        camera   (first (:camera/_selected entity))
        grid-type (:scene/grid-type (:camera/scene camera))
        anchor-editing? (= (:camera/draw-mode camera) :object-anchor)]
    (if anchor-editing?
      ;; While actively dragging the grid-anchor marker (see
      ;; object-board-edit in scene_objects.cljs), the confirm/cancel
      ;; controls render right next to the marker on the canvas instead
      ;; of here, so this menu steps out of the way entirely rather than
      ;; showing two separate floating controls over the same piece.
      nil
      ($ context-menu-fn
        {:render-toolbar
         (fn [{:keys [selected on-change]}]
           ($ :<>
             ($ :button
               {:type "button"
                :data-selected (= selected :rotation)
                :data-tooltip "Rotation"
                :on-click #(on-change :rotation)}
               ($ icon {:name "arrow-counterclockwise"}))
             ($ :button
               {:type "button"
                :data-tooltip "Save scale as default"
                :on-click
                (fn []
                  ;; Calibration reads a single entity's own scale/image,
                  ;; so a multi-selection just uses the first -- mirrors
                  ;; context-menu-prop's identical action.
                  (dispatch :image/set-cell-scale id))}
               ($ icon {:name "floppy"}))
             ($ :button
               {:type "button"
                :data-tooltip "Set grid anchor"
                :on-click
                (fn []
                  (dispatch :camera/change-mode :object-anchor))}
               ($ icon {:name "anchor"}))))
         :render-aside
         (fn []
           ($ :<>
             ($ action-hide
               {:value (every? :object/hidden data)
                :disabled (not (:host props))
                :on-change
                (fn []
                  (dispatch :objects/toggle-hidden-selected))})
             ($ action-lock
               {:value (every? :object/locked data)
                :disabled (not (:host props))
                :on-change
                (fn []
                  (dispatch :objects/toggle-locked-selected))})
             ($ action-remove
               {:on-click
                (fn []
                  (dispatch :objects/remove-selected))})))}
        (fn [{:keys [selected]}]
          (if (= selected :rotation)
            (for [[label mode] (board-rotation-options grid-type)]
              ($ :label.radio {:key (str mode)}
                ($ :input
                  {:type "radio"
                   :name "board-rotation-mode"
                   :checked (= (:object/rotation-mode entity) mode)
                   :on-change #(dispatch :object/change-rotation-mode id mode)})
                label))))))))

(defui context-menu [props]
  (if (util/uniform-by (comp namespace :object/type) (:data props))
    (case (namespace (:object/type (first (:data props))))
      "board" ($ context-menu-board props)
      "prop"  ($ context-menu-prop  props)
      "shape" ($ context-menu-shape props)
      "token" ($ context-menu-token props)
      nil)))
