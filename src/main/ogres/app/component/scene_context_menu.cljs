(ns ogres.app.component.scene-context-menu
  (:require [ogres.app.component :refer [icon]]
            [ogres.app.component.scene-pattern :refer [pattern]]
            [ogres.app.game-type :as game-type]
            [ogres.app.geom :as geom]
            [ogres.app.hooks :as hooks]
            [ogres.app.player :as player]
            [ogres.app.util :as util]
            [uix.core :as uix :refer [defui $]]))

(def ^:private size-units-per-cell
  "How many :token/size units make up one grid cell.

   :token/size is stored in fifths of a cell for historical reasons (it
   began life as D&D feet, where 5ft = one square). Nothing reads it as
   feet any more -- geom/object-bounding-rect derives a token's footprint
   as `size / 5` cells and component/scene scales the rendered circle by
   the same ratio -- so this is now just the encoding, and the controls
   below step it one whole CELL at a time."
  5)

(defn ^:private unit-size-label
  "Describes a token occupying `cells` grid cells.

   Generic: a footprint is always meaningful, so the fallback is a plain
   cell count. Two optional layers sit on top, each supplied by the
   active game-type rather than assumed here --

     * a real-world distance, if the game-type defines one (see
       :game-type/change-distance) -- \"10ft.\", \"4m\";
     * a name for that footprint, if some enabled element contributes
       :unit-size-labels -- D&D's \"Large\", \"Huge\".

   With both, D&D reads \"10ft. Large\" exactly as before; with neither,
   a game that simply has big and small units reads \"2 cells\"."
  [cells {:keys [per-cell unit]} labels]
  (let [distance (if (and (number? per-cell) (pos? per-cell) (seq unit))
                   (str (* cells per-cell) unit)
                   (str cells (if (= cells 1) " cell" " cells")))
        named (some (fn [[n label]] (if (= n cells) label)) labels)]
    (if named (str distance " " named) distance)))

(def ^:private enabled-elements-query
  [{:user/camera
    [{:camera/scene
      [{:scene/game-type
        [[:game-type/enabled-elements :default #{}]
         :game-type/distance-per-cell
         :game-type/distance-unit]}]}]}])

(defn ^:private use-enabled-elements []
  (let [result (hooks/use-query enabled-elements-query)]
    (-> result :user/camera :camera/scene :scene/game-type :game-type/enabled-elements)))

(defn ^:private use-unit-presentation
  "How the active game-type wants unit footprints described: its
   real-world distance definition (nil when it doesn't define one, in
   which case footprints read as bare cell counts) and any
   :unit-size-labels its enabled elements contribute. Both are read
   generically -- see game-type/elements' own docstring on the keys an
   element may declare."
  []
  (let [result (hooks/use-query enabled-elements-query)
        gt (-> result :user/camera :camera/scene :scene/game-type)
        enabled (:game-type/enabled-elements gt #{})]
    {:distance {:per-cell (:game-type/distance-per-cell gt)
                :unit (:game-type/distance-unit gt)}
     :size-labels (into [] (mapcat :unit-size-labels)
                        (vals (select-keys game-type/elements enabled)))}))

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
       :name "locked"
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
  (let [{:keys [distance size-labels]} (use-unit-presentation)
        value-fn (fn [attr] (first (into (sorted-set-by >) (values attr))))]
    ($ :<>
      (if (contains? enabled-elements :unit/size)
        ;; The primitive is "this unit occupies N grid cells", stepped a
        ;; whole cell at a time. Whether a cell is five feet, and whether
        ;; a 2x2 footprint is called "Large", are both supplied by the
        ;; active game-type -- see unit-size-label.
        (let [value (value-fn :token/size)
              cells (/ value size-units-per-cell)
              step (fn [n] (on-change :token/change-size
                                      (-> (+ value (* n size-units-per-cell))
                                          (max size-units-per-cell)
                                          (min (* 10 size-units-per-cell)))))]
          ($ :<>
            ($ :label "Size")
            ($ :button
              {:type "button"
               :auto-focus true
               :on-click #(step -1)
               :aria-label "Decrease token size by one cell"}
              "-")
            ($ :data {:value value}
              (unit-size-label cells distance size-labels))
            ($ :button
              {:type "button"
               :on-click #(step 1)
               :aria-label "Increase token size by one cell"} "+"))))
      (if (contains? enabled-elements :unit/light)
        (let [value (value-fn :token/light)]
          ($ :<>
            ($ :label "Light")
            ($ :button
              {:type "button"
               :on-click #(on-change :token/change-light (max (- value 5) 0))
               :aria-label "Decrease light radius by one cell"}
              "-")
            ($ :data {:value value}
              (if (> value 0)
                (str (unit-size-label (/ value size-units-per-cell) distance nil) " radius")
                "None"))
            ($ :button
              {:type "button"
               :on-click #(on-change :token/change-light (min (+ value 5) 120))
               :aria-label "Increase light radius by one cell"}
              "+"))))
      (if (contains? enabled-elements :unit/aura)
        (let [value (value-fn :token/aura-radius)]
          ($ :<>
            ($ :label "Aura")
            ($ :button
              {:type "button"
               :on-click #(on-change :token/change-aura (max (- value 5) 0))
               :aria-label "Decrease aura size by one cell"}
              "-")
            ($ :data {:value value}
              (if (> value 0)
                (str (unit-size-label (/ value size-units-per-cell) distance nil) " radius")
                "None"))
            ($ :button
              {:type "button"
               :on-click #(on-change :token/change-aura (min (+ value 5) 120))
               :aria-label "Increase aura size by one cell"}
              "+")))))))

(def ^:private owner-query
  [{:root/players [:db/id :player/name :player/kind]}
   {:root/token-images [:image/hash :image/name]}
   {:root/props-images [:image/hash :image/name]}])

(defui ^:private token-form-owner
  "Shared by both the token and prop context menus (see context-menu-
   token/context-menu-prop's :owner toolbar tab) -- assigns which
   roster player owns the selected objects (:objects/assign-owner) and,
   separately, a placeholder image shown in place of the real one to a
   viewer who lacks authority to see it while hidden
   (:objects/assign-alt-image, see resolve-hidden in scene_objects.cljs).
   `object-type` (:token/token or :prop/prop) picks which image library
   the placeholder picker offers, matching the object's own image
   namespace (:token/image-alt vs :prop/image-alt). Also sets the
   :object/shared? 'public toggle' flag (:objects/assign-shared) --
   letting ANY connected participant, not just the owner's controller,
   flip the object's hidden state (see authorized-to-hide?,
   events.cljs). This checkbox lives here (host-only, alongside Owner)
   deliberately: a guest can never mark something shared themselves,
   only flip an object the host has already marked shared."
  [{:keys [object-type values on-change]
    :or   {values (constantly (list)) on-change identity}}]
  (let [result (hooks/use-query owner-query [:db/ident :root])
        players (:root/players result)
        token? (= object-type :token/token)
        images (if token? (:root/token-images result) (:root/props-images result))
        alt-key (if token? :token/image-alt :prop/image-alt)
        owner-ids (values (comp :db/id :object/owner))
        owner-id (if (= (count owner-ids) 1) (first owner-ids) nil)
        alt-hashes (values (comp :image/hash alt-key))
        alt-hash (if (= (count alt-hashes) 1) (first alt-hashes) nil)
        shared? (= (values :object/shared?) #{true})]
    ($ :<>
      ($ :label "Owner")
      ($ :select
        {:value (or owner-id "")
         :on-change
         (fn [event]
           (let [v (.. event -target -value)]
             (on-change :objects/assign-owner (if (seq v) (js/Number v) nil))))}
        ($ :option {:value ""} "Unassigned")
        (for [p players]
          ($ :option {:key (:db/id p) :value (:db/id p)} (:player/name p))))
      ($ :label "Placeholder image")
      ($ :select
        {:value (or alt-hash "")
         :on-change
         (fn [event]
           (let [v (.. event -target -value)]
             (on-change :objects/assign-alt-image alt-key (if (seq v) v nil))))}
        ($ :option {:value ""} "None")
        (for [img images]
          ($ :option {:key (:image/hash img) :value (:image/hash img)} (:image/name img))))
      ($ :label "Shared")
      ($ :input
        {:type "checkbox"
         :checked shared?
         :on-change
         (fn [event]
           (on-change :objects/assign-shared (.. event -target -checked)))}))))

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
                         (:host props) (conj [:owner "person-circle" "Owner"])
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
              :disabled
              ;; Not just `(:host props)` -- player/authority? already
              ;; falls back to host when unassigned/disconnected; ORing
              ;; host in here too would wrongly let the host bypass a
              ;; *connected* controller's exclusive authority, defeating
              ;; the point of hiding something from the host. Uses
              ;; :default-authority, not the raw :host, as the fallback
              ;; -- suppressed on a :scene/neutral-authority? scene (see
              ;; scene_objects.cljs), so the host doesn't get an
              ;; automatic pass over gameplay state there either. A
              ;; :object/shared? entity is the one explicit exception --
              ;; ANY connected participant (including the host) may
              ;; always toggle it.
              (not (every? #(or (:object/shared? %)
                                 (player/authority?
                                  (:viewer-uuid props) (:default-authority props) (:connected-uuids props)
                                  (get-in % [:object/owner :player/controller :user/uuid])))
                            data))
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
            :owner   ($ token-form-owner (assoc props :object-type :token/token))
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
        idxs     (into [] (map :db/id) data)
        entity   (first data)
        id       (:db/id entity)
        camera   (first (:camera/_selected entity))
        anchor-editing? (= (:camera/draw-mode camera) :object-anchor)
        ;; "Draw" only makes sense when the whole selection is exactly
        ;; one physical pile -- a single, non-nil, shared :pile/id (see
        ;; ogres.app.props/pile) across every selected prop.
        pile-ids (into #{} (map (comp :pile/id :object/variables)) data)
        pile-id (if (and (= (count pile-ids) 1) (some? (first pile-ids))) (first pile-ids))
        ;; A single selected Memory card (see ogres.app.memory) routes
        ;; the familiar hide/reveal control through the turn-gated
        ;; :memory/flip event instead of the generic :objects/toggle-
        ;; hidden-selected -- every Memory card is already
        ;; :object/shared? true, so the generic authority check would
        ;; pass for anyone regardless of turn; :memory/flip is what
        ;; actually enforces 'players take turns'.
        memory-value (and (= (count data) 1) (:memory/value (:object/variables entity)))]
    (if anchor-editing?
      ;; While actively dragging the grid-anchor marker (see
      ;; object-prop-edit in scene_objects.cljs), the confirm/cancel
      ;; controls render right next to the marker on the canvas instead
      ;; of here, so this menu steps out of the way entirely rather than
      ;; showing two separate floating controls over the same piece.
      nil
      ($ context-menu-fn
        {:render-toolbar
         (fn [{:keys [selected on-change]}]
           ($ :<>
             (if (:host props)
               ($ :button
                 {:type "button"
                  :data-selected (= selected :owner)
                  :data-tooltip "Owner"
                  :on-click #(on-change :owner)}
                 ($ icon {:name "person-circle"})))
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
               ($ icon {:name "anchor"}))
             (if pile-id
               ($ :button
                 {:type "button"
                  :data-tooltip "Draw"
                  :on-click
                  (fn []
                    ;; target-point nil -- draws in place, un-piling the
                    ;; top card without moving it; flipping it face-up is
                    ;; a separate action via the hide/reveal control.
                    (dispatch :props/draw-from-pile pile-id nil))}
                 ($ icon {:name "arrow-up-short"})))))
         :render-aside
         (fn []
           ($ :<>
             ($ action-hide
               {:value (every? :object/hidden data)
                :disabled
                (if memory-value
                  ;; Already face-up -- only :memory/resolve may re-hide
                  ;; it (on a mismatch); manual re-hiding is disallowed.
                  ;; Left ENABLED while face-down for any connected
                  ;; viewer -- :memory/flip itself is what enforces
                  ;; whose turn it is, this is just a UX courtesy, not
                  ;; the real gate.
                  (not (:object/hidden entity))
                  ;; Not just `(:host props)` -- see the identical comment
                  ;; in context-menu-token's action-hide; the same
                  ;; exclusive-controller/shared?/:default-authority
                  ;; logic applies to props.
                  (not (every? #(or (:object/shared? %)
                                     (player/authority?
                                      (:viewer-uuid props) (:default-authority props) (:connected-uuids props)
                                      (get-in % [:object/owner :player/controller :user/uuid])))
                                data)))
                :on-change
                (fn []
                  (if memory-value
                    (dispatch :memory/flip id)
                    (dispatch :objects/toggle-hidden-selected)))})
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
        (fn [{:keys [selected on-change]}]
          (if (= selected :owner)
            ($ token-form-owner
               {:object-type :prop/prop
                :on-close  #(on-change nil)
                :on-change #(apply dispatch %1 idxs %&)
                :values    (fn vs
                             ([f] (vs f #{}))
                             ([f init] (into init (map f) data)))})))))))

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
