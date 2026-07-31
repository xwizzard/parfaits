(ns ogres.app.component.panel-scene
  (:require [clojure.string :refer [replace]]
            [ogres.app.component :as component :refer [icon]]
            [ogres.app.const :refer [grid-size]]
            [ogres.app.game-type :as game-type]
            [ogres.app.geom :as geom]
            [ogres.app.hooks :as hooks]
            [ogres.app.segment :as seg]
            [ogres.app.util :refer [display-size]]
            [ogres.app.vec :as vec :refer [Vec2]]
            [uix.core :as uix :refer [defui $]]
            [uix.dom :refer [create-portal]]
            ["@dnd-kit/core" :as dnd]
            ["@dnd-kit/modifiers" :as modifiers]))

(def ^:private query
  [{:root/scene-images
    [:db/id
     :image/hash
     :image/name
     :image/size
     {:image/thumbnail
      [:image/hash]}]}
   :root/default-cell-px
   {:root/user
    [{:user/camera
      [:db/id
       :camera/label
       {:camera/scene
        [:db/id
         [:scene/grid-size :default grid-size]
         [:scene/grid-type :default :square]
         [:scene/grid-shape :default :line]
         [:scene/show-grid :default true]
         [:scene/grid-align :default false]
         [:scene/dark-mode :default false]
         [:scene/neutral-authority? :default false]
         [:scene/show-object-outlines :default true]
         [:scene/lighting :default :revealed]
         [:scene/token-scale :default {}]
         [:scene/grid-order-board :default :under]
         [:scene/grid-order-props :default :under]
         {:scene/board
          [{:board/image
            [:image/name
             {:image/thumbnail
              [:image/hash]}]}]}
         {:scene/game-type
          [[:game-type/enabled-elements :default #{}]]}]}]}]}])

(def ^:private options-vis
  [["Revealed" :revealed "sun-fill"]
   ["Obscured" :dimmed "cloud-sun-fill"]
   ["Hidden" :hidden "moon-fill"]])

(def ^:private options-grid-type
  ;; Iso variants get their own dedicated icons (a squished/rotated single
  ;; cell matching geom.cljs's actual iso-square-forward/iso-hex-forward
  ;; transforms) rather than reusing the flat, unprojected square/hexagon
  ;; icons -- so the radio list itself hints at which options are skewed.
  [["Square" :square "square-cell"]
   ["Hex (Pointy)" :hex-pointy "hexagon"]
   ["Hex (Flat)" :hex-flat "hexagon-flat"]
   ["Iso Square" :iso-square "square-iso"]
   ["Iso Hex (Pointy)" :iso-hex-pointy "hexagon-iso"]
   ["Iso Hex (Flat)" :iso-hex-flat "hexagon-flat-iso"]
   ["Iso Square (Vertical)" :iso-square-vertical "square-iso-vertical"]
   ["Iso Hex Pointy (Vertical)" :iso-hex-pointy-vertical "hexagon-iso-vertical"]
   ["Iso Hex Flat (Vertical)" :iso-hex-flat-vertical "hexagon-flat-iso-vertical"]])

(defn ^:private grid-type-options [enabled-elements]
  (filter (fn [[_ value]] (contains? enabled-elements (game-type/grid-tool-id value)))
          options-grid-type))

(defn ^:private grid-type-label [grid-type]
  (or (first (first (filter (fn [[_ value]] (= value grid-type)) options-grid-type)))
      "Square"))

(def ^:private options-grid-shape
  ;; "None" replaces the "Show grid" checkbox that used to sit on its own
  ;; in the "Grid options" fieldset below -- folding that boolean into
  ;; this radio group means grid visibility and grid style are one
  ;; choice instead of two, so on-change wiring in the render loop below
  ;; turns show-grid back on for every value but this one. See also
  ;; [[options-grid-type]] for the same "dedicated small icon" treatment
  ;; applied to "Octo" here (shares a name with the toolbar's full-bleed
  ;; square-draw tool, which must stay full-bleed).
  [["None" :none "no"]
   ["Lines" :line "dash"]
   ["Dots" :dot "dot"]
   ["Circles" :circle "circle"]
   ["Octo" :octo "square-cell"]
   ["Hex" :hex "hexagon"]])

(defn ^:private grid-shape-options [grid-type]
  ;; "Octo" markers are only meaningful on square-family grids (Square,
  ;; Iso Square, Iso Square (Vertical)), and "Hex" markers only on
  ;; hex-family grids (Hex/Iso Hex, Pointy or Flat) -- neither shape has
  ;; an analogous reading on the other lattice, so the mismatched option
  ;; isn't offered at all rather than being selectable but silently
  ;; ignored (or silently substituted for something else). "None" is
  ;; always offered, on either lattice.
  (let [square? (= (geom/base-grid-type grid-type) :square)]
    (remove (fn [[_ value]] (or (and (= value :octo) (not square?))
                                 (and (= value :hex) square?)))
            options-grid-shape)))

(def ^:private options-grid-order
  [["Over" :over] ["Under" :under]])

(def ^:private per-page 6)

(def ^:private filesize-limit 8e6)

(def ^:private filename-re #"\d+x\d+|[^\w ]|.[^.]+$")

(defn ^:private render-scene-name [camera]
  (if-let [label (:camera/label camera)]
    label
    ;; No custom label -- fall back to the name of the scene's first
    ;; placed board piece (there's no longer one single background image
    ;; to name the scene after).
    (if-let [filename (-> camera :camera/scene :scene/board first :board/image :image/name)]
      (-> filename (replace filename-re "") (replace  #"\s{2,}" " "))
      "Untitled scene")))

(def ^:private query-scenes
  [{:root/scene-images
    [:db/id
     :image/hash
     :image/name
     :image/size
     {:image/thumbnail
      [:image/hash]}]}])

(defui ^:private scene-editor [props]
  (let [result (hooks/use-query query-scenes [:db/ident :root])
        scene  (first (filter (comp #{(:id props)} :db/id) (:root/scene-images result)))]
    ($ component/fullscreen-dialog
      {:on-close (:on-close props)}
      ($ :.scene-editor
        ($ :.scene-editor-workspace
          ($ component/image {:hash (:image/hash scene)}
            (fn [url]
              ($ :img.scene-editor-image
                {:src url})))
          ($ :dl
            ($ :dt "Filename")
            ($ :dd (:image/name scene))
            ($ :dt "Size")
            ($ :dd (display-size (:image/size scene)))
            (if (> (:image/size scene) filesize-limit)
              ($ :<>
                ($ :dt ($ icon {:name "exclamation-triangle-fill" :size 12}) "Warning")
                ($ :dd
                  "This image exceeds the maximum image filesize (8MB)"
                  " that can be used for multiplayer games."
                  " Reducing its dimensions and lowering its image quality "
                  " may help.")))))
        ($ :.scene-editor-gallery
          ($ component/paginated
            {:data (:root/scene-images result) :page-size 12}
            (fn [{:keys [data pages page on-change]}]
              ($ :.scene-editor-gallery-paginated
                ($ :.scene-editor-gallery-thumbnails
                  (for [scene data
                        :let [thumb (:image/hash (:image/thumbnail scene))
                              check (= (:db/id scene) (:id props))]]
                    ($ component/image {:key thumb :hash thumb}
                      (fn [url]
                        ($ :label
                          {:style {:background-image (str "url(" url ")")}}
                          ($ :input
                            {:type "radio"
                             :name "scene-image-editor"
                             :value (:db/id scene)
                             :checked check
                             :on-change
                             (fn [event]
                               ((:on-change props) (js/Number (.. event -target -value))))}))))))
                (if (> pages 1)
                  ($ component/pagination
                    {:name "scenes-editor"
                     :label "Scene editor images pages"
                     :class-name "dark"
                     :pages pages
                     :value page
                     :on-change on-change})))))
          ($ :button.token-editor-button
            {:on-click (:on-close props)}
            "Exit"))))))

;; Placing a board piece is drag-and-drop from a gallery thumbnail onto
;; the canvas, the same mechanic Props' own gallery already uses
;; (panel_props.cljs) -- a piece's default size/rotation and, once
;; calibrated, its default scale are handled by :board/create itself; this
;; is just the thumbnail-drag plumbing that calls it.
(defui ^:private board-gallery-thumbnail [props]
  (let [{data :data on-preview :on-preview} props
        {hash :image/hash} data
        thumb (:image/hash (:image/thumbnail data))
        dispatch (hooks/use-dispatch)
        opt (dnd/useDraggable #js {"id" hash "data" #js {"hash" hash}})]
    ($ component/image {:hash thumb}
      (fn [url]
        ($ :fieldset.scene-gallery-thumbnail
          {:data-type "image" :style {:background-image (str "url(" url ")")}}
          ($ :button.scene-gallery-thumbnail-drag
            {:ref (.-setNodeRef opt)
             :aria-label (str "Place " (:image/name data))
             :on-pointer-down (.. opt -listeners -onPointerDown)
             :on-key-down (.. opt -listeners -onKeyDown)})
          ($ :button.button.button-neutral
            {:type "button"
             :name "info"
             :aria-label "Preview"
             :on-click
             (fn [event]
               (.stopPropagation event)
               (on-preview (:db/id data)))}
            ($ icon {:name "zoom-in" :size 18}))
          ($ :button.button.button-danger
            {:type "button"
             :name "remove"
             :aria-label "Remove"
             :on-click
             (fn [event]
               (.stopPropagation event)
               (dispatch :scene-images/remove hash thumb))}
            ($ icon {:name "trash3-fill" :size 18}))
          (if (> (:image/size data) filesize-limit)
            ($ :button.button.button-warning
              {:type "button"
               :name "warn"
               :aria-label "Exceeds filesize limit"
               :data-tooltip "Exceeds filesize limit"
               :on-click
               (fn [event]
                 (.stopPropagation event)
                 (on-preview (:db/id data)))}
              ($ icon {:name "exclamation-triangle-fill" :size 18}))))))))

(defui ^:private ^:memo board-gallery-overlay []
  (let [[active set-active] (uix/use-state nil)
        url (hooks/use-image active)]
    (dnd/useDndMonitor
     #js {"onDragStart" (fn [event] (set-active (.. event -active -data -current -hash)))
          "onDragEnd"   (fn [_]     (set-active nil))})
    (create-portal
     ($ dnd/DragOverlay
       {:modifiers #js [modifiers/snapCenterToCursor]
        :drop-animation nil}
       ($ :img.scene-gallery-overlay-content {:src url}))
     js/document.body)))

(defui ^:memo panel []
  (let [[preview set-preview] (uix/use-state nil)
        dispatch (hooks/use-dispatch)
        upload   (hooks/use-image-uploader {:type :scene})
        input    (uix/use-ref)
        data     (hooks/use-query query [:db/ident :root])
        {{{scene :camera/scene} :user/camera
          camera :user/camera} :root/user
         default-cell-px :root/default-cell-px} data
        enabled-elements (:game-type/enabled-elements (:scene/game-type scene) #{})
        ;; "No-grid mode": the active game-type has no grid layout enabled
        ;; at all, so the scene is on an invisible, unaligned square grid
        ;; (see :game-type/toggle-element). Image scale / grid options /
        ;; grid alignment are all meaningless with no grid to configure.
        no-grid? (zero? (game-type/grid-count enabled-elements))
        ;; The scene-level Lighting mode and each token's individual light
        ;; radius (scene_context_menu.cljs) are two aspects of the same
        ;; system -- disabling :unit/light hides both together, the same
        ;; way disabling every grid layout hides both the grid-type picker
        ;; and its alignment/visibility options above.
        light-enabled? (contains? enabled-elements :unit/light)
        grid-type (:scene/grid-type scene :square)
        ;; Token scale is remembered per grid-type (not one value for the
        ;; whole scene) so a host can tune token size against each grid
        ;; type's own geometry -- switching grid types to compare doesn't
        ;; clobber the scale already tuned for the one switched away from.
        token-scale-pct (get (:scene/token-scale scene {}) grid-type 100)]
    ($ :form.form-scenes
      {:on-submit (fn [event] (.preventDefault event))}
      ($ :header ($ :h2 "Scene"))
      ($ :fieldset.fieldset
        ($ :legend "Name")
        ($ :input.text.text-ghost
          {:type "text"
           :maxLength 36
           :spellCheck false
           :placeholder (render-scene-name camera)
           :value (or (:camera/label camera) "")
           :on-change
           (fn [event]
             (let [value (.. event -target -value)]
               (if (not= value "")
                 (dispatch :camera/change-label value)
                 (dispatch :camera/remove-label))))}))
      ($ :fieldset.fieldset
        ($ :legend "Background image")
        ($ :.form-notice
          "Drag a thumbnail onto the scene to place it as a board piece --
           the game board, play mat, or map. Board pieces are set up in
           advance and are meant to stay put during play; select a placed
           piece on the canvas for its own menu of rotate, scale, hide,
           lock, and grid-calibration options.")
        ($ dnd/DndContext
          #js {"onDragEnd"
               (fn [event]
                 (let [bound (seg/DOMRect-> (.getBoundingClientRect (.. event -activatorEvent -target)))
                       delta (Vec2. (.-x (.-delta event)) (.-y (.-delta event)))
                       hash (.. event -active -data -current -hash)]
                   (dispatch :board/create (vec/add (.-a bound) delta) hash)))}
          ($ component/paginated
            {:data (:root/scene-images data) :page-size 6}
            (fn [{:keys [data pages page on-change]}]
              (let [data (->> (repeat per-page :placeholder) (into data) (take per-page) (map-indexed vector))]
                ($ :<>
                  ($ :.scene-gallery
                    (for [[idx data] data]
                      (if (:image/hash (:image/thumbnail data))
                        ($ board-gallery-thumbnail {:key idx :data data :on-preview set-preview})
                        ($ :.scene-gallery-thumbnail {:key idx :data-type "placeholder"}))))
                  ($ :fieldset.scene-gallery-form
                    ($ :button.button.button-neutral
                      {:type "button" :on-click #(.click (deref input))}
                      ($ :input
                        {:ref input
                         :type "file"
                         :hidden true
                         :accept "image/*"
                         :multiple true
                         :on-change
                         (fn [event]
                           (upload (.. event -target -files))
                           (set! (.. event -target -value) ""))})
                      ($ icon {:name "camera-fill" :size 16}) "Upload images")
                    ($ component/image-url-form {:type :scene})
                    (if (> pages 1)
                      ($ component/pagination
                        {:name "scenes-gallery"
                         :label "Scene image pages"
                         :pages pages
                         :value page
                         :on-change on-change})))))))
          ($ board-gallery-overlay))
        (if (some? preview)
          ($ scene-editor
            {:id preview
             :on-change set-preview
             :on-close (fn [] (set-preview nil))})))
      (let [available (grid-type-options enabled-elements)]
          ;; A radio list with zero or one option is nothing to choose --
          ;; hide it entirely rather than show a foregone conclusion. The
          ;; active game-type determines which grid layouts even apply.
          (if (> (count available) 1)
            ($ :fieldset.fieldset.fieldset--radio
              ($ :legend "Grid type")
              ($ :.input-group
                (for [[label value icon-name] available
                      :let [on-change #(dispatch :scene/change-grid-type value)]]
                  ($ :<> {:key value}
                    ($ :label.radio
                      ($ :input
                        {:type "radio"
                         :name "grid-type"
                         :value value
                         :checked (= (:scene/grid-type scene) value)
                         :on-change on-change})
                      ($ icon {:name icon-name :size 16})
                      label))))
              ($ :details
                ($ :summary "More Information")
                "Square grids suit most tabletop systems. Hex grids match the
                 layout of games like Gloomhaven and Frosthaven."))))
      (let [set-scale (fn [value] (dispatch :scene/change-token-scale grid-type (max 25 (min 400 value))))]
        ($ :fieldset.fieldset
          ($ :legend (str "Token scale ( " (grid-type-label grid-type) " )"))
          ($ :.scene-token-scale
            ($ :button.button.button-neutral
              {:type "button"
               :on-click #(set-scale (- token-scale-pct 5))
               :aria-label "Decrease token scale by 5%"}
              ($ icon {:name "dash" :size 16}))
            ($ :input.text.text-ghost
              {:type "number"
               :name "Token scale"
               :min 25
               :max 400
               :step 5
               :value token-scale-pct
               :placeholder "100%"
               :on-change
               (fn [event]
                 (let [value (.. event -target -value)]
                   (if (not= value "")
                     (set-scale (js/Number value)))))})
            ($ :button.button.button-neutral
              {:type "button"
               :on-click #(set-scale (+ token-scale-pct 5))
               :aria-label "Increase token scale by 5%"}
              ($ icon {:name "plus" :size 16})))
          ($ :details
            ($ :summary "More Information")
            "Uniformly scales the base size of every token on this scene, as
             a percentage of its default size. Each grid type remembers its
             own token scale, so switching grid types to compare doesn't
             lose the value tuned for the one switched away from.")))
      ($ :fieldset.fieldset.fieldset--radio
        ($ :legend "Grid style")
        ($ :.input-group
          (for [[label value icon-name] (grid-shape-options grid-type)
                :let [checked (if (:scene/show-grid scene)
                                (= (:scene/grid-shape scene) value)
                                (= value :none))
                      on-change
                      (fn []
                        (if (= value :none)
                          (dispatch :scene/toggle-show-grid false)
                          (do (dispatch :scene/change-grid-shape value)
                              (dispatch :scene/toggle-show-grid true))))]]
            ($ :<> {:key value}
              ($ :label.radio
                ($ :input
                  {:type "radio"
                   :name "grid-shape"
                   :value value
                   :checked checked
                   :on-change on-change})
                ($ icon {:name icon-name :size 16})
                label))))
        ($ :details
          ($ :summary "More Information")
          "None hides the grid entirely. Lines draw the full grid. Dots
           mark only the position each token or shape will snap to.
           Circles, Octo (square grids only), and Hex (hex grids only)
           mark that same position at nearly the size of the grid cell,
           useful for previewing how tokens will fit before placing any."))
      (if-not no-grid?
        ($ :fieldset.fieldset
          ($ :legend "Image scale ( px per cell )")
          ($ :input.text.text-ghost
            {:type "number"
             :name "Image scale"
             :min 1
             :value (or default-cell-px "")
             :placeholder "native size"
             :on-change
             (fn [event]
               (let [value (.. event -target -value)]
                 (dispatch :root/change-default-cell-px
                           (if (= value "") 0 (js/Number value)))))})
          ($ :details
            ($ :summary "More Information")
            "How many pixels of a map or prop image make up one grid cell.
             Set it once and every board piece and prop you place afterwards
             drops already scaled to the grid, instead of at its native
             pixel size. Leave it empty to place images at native size.

             This is a baseline for a whole asset set. An individual image
             can still override it: scale one placed copy until it fits,
             then use "
            ($ :strong "Save scale as default")
            " on it -- that calibration wins over this value, and also
             corrects every copy of that image already on your scenes.

             Images already placed are not affected; this only changes how
             new ones land.")))
      (if-not no-grid?
        ($ :fieldset.fieldset
          ($ :legend "Grid options")
          ($ :.input-group
            ($ :label.checkbox
              ($ :input
                {:type "checkbox"
                 :checked (:scene/show-object-outlines scene)
                 :on-change #(dispatch :scene/toggle-object-outlines (.. % -target -checked))})
              ($ icon {:name "check" :size 20})
              "Show shape outlines")
            ($ :label.checkbox
              ($ :input
                {:type "checkbox"
                 :checked (:scene/grid-align scene)
                 :on-change #(dispatch :scene/toggle-grid-align (.. % -target -checked))})
              ($ icon {:name "check" :size 20})
              "Align to grid")
            ($ :label.checkbox
              ($ :input
                {:type "checkbox"
                 :checked (:scene/dark-mode scene)
                 :on-change #(dispatch :scene/toggle-dark-mode (.. % -target -checked))})
              ($ icon {:name "check" :size 20})
              "Use dark grid"))))
      ($ :fieldset.fieldset
        ($ :legend "Visibility")
        ($ :.input-group
          ($ :label.checkbox
            ($ :input
              {:type "checkbox"
               :checked (:scene/neutral-authority? scene)
               :on-change #(dispatch :scene/toggle-neutral-authority (.. % -target -checked))})
            ($ icon {:name "check" :size 20})
            "Impartial dealer (host doesn't auto-see hidden objects)")))
      (if-not no-grid?
        ($ :fieldset.fieldset.fieldset--radio
          ($ :legend "Grid vs. board")
          ($ :.input-group
            (for [[label value] options-grid-order
                  :let [on-change #(dispatch :scene/change-grid-order-board value)]]
              ($ :label.radio {:key value}
                ($ :input
                  {:type "radio"
                   :name "grid-order-board"
                   :checked (= (:scene/grid-order-board scene) value)
                   :on-change on-change})
                label)))
          ($ :details
            ($ :summary "More Information")
            "Whether the grid lines render over or under the placed board
             (map/background) pieces. The board is always the bottommost
             layer regardless of this setting -- it only changes whether
             the grid is visible on top of it.")))
      (if-not no-grid?
        ($ :fieldset.fieldset.fieldset--radio
          ($ :legend "Grid vs. props")
          ($ :.input-group
            (for [[label value] options-grid-order
                  :let [on-change #(dispatch :scene/change-grid-order-props value)]]
              ($ :label.radio {:key value}
                ($ :input
                  {:type "radio"
                   :name "grid-order-props"
                   :checked (= (:scene/grid-order-props scene) value)
                   :on-change on-change})
                label)))
          ($ :details
            ($ :summary "More Information")
            "Whether the grid lines render over or under props and tokens.
             Props and tokens always render above the board regardless of
             this setting -- it only changes whether the grid is visible on
             top of them.")))
      (if light-enabled?
        ($ :fieldset.fieldset.fieldset--radio
          ($ :legend "Lighting")
          ($ :.input-group
            (for [[label value icon-name] options-vis
                  :let [on-change #(dispatch :scene/change-lighting value)]]
              ($ :<> {:key value}
                ($ :label.radio
                  ($ :input
                    {:type "radio"
                     :name "visi"
                     :value value
                     :checked (= (:scene/lighting scene) value)
                     :on-change on-change})
                  ($ icon {:name icon-name :size 16})
                  label))))
          ($ :details
            ($ :summary "More Information")
            "When set to " ($ :strong "Obscured") " or " ($ :strong "Hidden")
            ", the scene will be shrouded in darkness and tokens will emit a
             radius of light around themselves. The light radius of each token
             can be customized.")))
      (if-not no-grid?
        ($ :fieldset.fieldset.scene-gallery-grid-align
          ($ :legend "Grid alignment")
          ($ :div.form-notice
            ($ :button
              {:on-click #(dispatch :camera/change-mode :grid)}
              ($ icon {:name "compass" :size 22}))
            "Use the grid alignment tool to manually pick a point that will serve
             as the origin of the grid. Use this tool when the grid in your scene
             image is offset from the edges or is unevenly distributed.")
          ($ :details
            ($ :summary "How To Use")
            ($ :ol
              ($ :li "Select the grid alignment tool.")
              ($ :li "Select a corner of one of the tiles in the scene.")
              ($ :li "Adjust its position carefully if necessary.")
              ($ :li "Adjust the tile size until the grid lines match up with the lines on the scene."))
            "Sometimes the widths of the tiles in the image are not whole
             numbers; it may be necessary to use this tool again in another
             part of the image as the adventure progresses there."))))))
