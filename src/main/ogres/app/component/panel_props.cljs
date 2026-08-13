(ns ogres.app.component.panel-props
  (:require [ogres.app.component :as component :refer [icon]]
            [ogres.app.hooks :as hooks]
            [ogres.app.library :as library]
            [ogres.app.provider.state :as state]
            [ogres.app.segment :as seg]
            [ogres.app.vec :as vec :refer [Vec2]]
            [uix.core :as uix :refer [defui $]]
            [uix.dom :refer [create-portal]]
            ["@dnd-kit/core" :as dnd]
            ["@dnd-kit/modifiers" :as modifiers]))

(def ^:private query
  [{:root/props-images
    [:image/hash
     :image/name
     :image/width
     :image/height
     {:image/thumbnail
      [:image/hash]}]}])

(def ^:private query-actions
  [{:root/user [:user/host]}
   {:root/props-images
    [:image/hash
     :image/name
     :image/width
     :image/height
     :image/size
     :image/cell-px
     :image/rotation
     :image/anchor]}])

(defui ^:private element [props]
  (let [{{hash :image/hash
          {thumb :image/hash}
          :image/thumbnail} :image} props
        url (hooks/use-image thumb)
        opt (dnd/useDraggable #js {"id" hash "data" #js {"hash" hash}})]
    ($ :button.props-gallery-image
      {:ref (.-setNodeRef opt)
       :style {:background-image (str "url(" url ")")}
       :on-pointer-down (.. opt -listeners -onPointerDown)
       :on-key-down (.. opt -listeners -onKeyDown)})))

(defui ^:private ^:memo overlay []
  (let [[active set-active] (uix/use-state nil)
        url (hooks/use-image active)]
    (dnd/useDndMonitor
     #js {"onDragStart" (fn [event] (set-active (.. event -active -data -current -hash)))
          "onDragEnd"   (fn [_]     (set-active nil))})
    (create-portal
     ($ dnd/DragOverlay
       {:modifiers #js [modifiers/snapCenterToCursor]
        :drop-animation nil}
       ($ :img.props-gallery-overlay-content
         {:src url}))
     js/document.body)))

(defui ^:private ^:memo gallery []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])]
    ($ component/paginated
      {:data (:root/props-images result) :page-size 16}
      (fn [{:keys [data pages page on-change]}]
        ($ :fieldset.fieldset.props-gallery
          ($ :legend "Images")
          ($ :.props-gallery-page
            (for [[idx term] (->> (repeat :placeholder) (concat data) (take 16) (map-indexed vector))]
              (if (keyword? term)
                ($ :fieldset.props-gallery-placeholder
                  {:key idx})
                ($ :fieldset.props-gallery-fieldset
                  {:key (:image/hash term)}
                  ($ element {:image term})
                  ($ :button.button.button-danger.props-gallery-remove
                    {:on-click
                     (fn []
                       (dispatch :props-images/remove (:image/hash term)))}
                    ($ icon {:name "trash3-fill" :size 16}))))))
          (if (> pages 1)
            ($ :.props-gallery-pagination
              ($ component/pagination
                {:name "props-images"
                 :label "Prop images pages"
                 :pages pages
                 :value page
                 :on-change on-change}))))))))

(defui ^:private ^:memo container []
  (let [dispatch (hooks/use-dispatch)]
    ($ dnd/DndContext
      #js {"onDragEnd"
           (fn [event]
             (let [bound (seg/DOMRect-> (.getBoundingClientRect (.. event -activatorEvent -target)))
                   delta (Vec2. (.-x (.-delta event)) (.-y (.-delta event)))
                   image (.. event -active -data -current -hash)]
               (dispatch :props/create (vec/add (.-a bound) delta) image)))}
      ($ gallery)
      ($ overlay))))

(defui ^:memo panel []
  ($ :.form-props
    ($ :header ($ :h2 "Props"))
    ($ container)
    ($ :.form-notice
      "Props are images you upload from your computer and use in the scene as
       decoration, vehicles, or effects. Once placed within the scene, they can
       be resized, rotated, and moved around freely."
      ($ :br)
      ($ :br)
      ($ :strong "Locking props ") "will prevent them from being accidentally "
      "moved around. To unlock them, " ($ :strong "double-click") " it to "
      "select it. "
      ($ :strong "Removing images ") "will also remove them from all scenes.")))

(def ^:private query-camera
  [{:user/camera [[:camera/point :default vec/zero]]}])

(defui ^:memo actions []
  (let [[overwrite? set-overwrite!] (uix/use-state false)
        [notice set-notice!] (uix/use-state nil)
        dispatch (hooks/use-dispatch)
        upload (hooks/use-image-uploader {:type :props})
        input (uix/use-ref)
        import-input (uix/use-ref)
        {{point :camera/point} :user/camera} (hooks/use-query query-camera)
        {{host :user/host} :root/user
         images :root/props-images} (hooks/use-query query-actions [:db/ident :root])]
    ($ :<>
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
      ($ :button.button.button-neutral
        {:on-click (fn [] (.click @input))}
        ($ icon {:name "camera-fill" :size 16})
        "Upload images")
      ($ component/image-url-form {:type :props})
      ($ :button.button.button-neutral
        {:title "Stamps 4 face-down demo cards onto the scene as a
                 physical pile -- proves out the prop-copy/pile
                 mechanism (:props/create-pile). Any connected
                 participant may flip one (see the Owner tab's 'Shared'
                 checkbox), and a selected pile shows a 'Draw' action."
         :on-click
         (fn []
           (dispatch :props/create-pile point state/card-back-hash 4 (random-uuid)
                     {:alt-hash state/card-front-hash :hidden? true :shared? true}))}
        ($ icon {:name "card-back" :size 16})
        "Add Card Pile (Demo)")
      ($ :button.button.button-danger
        {:on-click (fn [] (dispatch :props-images/remove-all))}
        ($ icon {:name "trash3-fill" :size 16}))
      ($ :button.button.button-neutral
        {:type "button"
         :title "Export a reusable library of every prop image, including
                 its calibrated scale/rotation/anchor, for reuse in a
                 future session"
         :disabled (not (seq images))
         :on-click
         (fn []
           (library/export! :props "props" (map library/image->entry images)))}
        ($ icon {:name "box-arrow-up-right" :size 16}) "Export library")
      (if host
        ($ :<>
          ($ :button.button.button-neutral
            {:type "button"
             :title "Import a prop image library exported from a previous
                     session"
             :on-click #(.click (deref import-input))}
            ($ icon {:name "door-open" :size 16}) "Import library"
            ($ :input
              {:ref import-input
               :type "file"
               :hidden true
               :accept ".props-library,text/plain"
               :on-change
               (fn [event]
                 (if-let [file (first (array-seq (.. event -target -files)))]
                   (library/import!
                    :props file
                    (fn [manifest]
                      (if manifest
                        (do (dispatch :image-library/import manifest overwrite?)
                            (set-notice! (str "Imported " (count (:entries manifest))
                                               " image(s) from \"" (:name manifest) "\".")))
                        (set-notice! "That file isn't a valid props image library.")))))
                 (set! (.. event -target -value) ""))}))
          ($ :label.checkbox
            ($ :input
              {:type "checkbox"
               :checked overwrite?
               :on-change #(set-overwrite! (.. % -target -checked))})
            ($ icon {:name "check" :size 20})
            "Overwrite existing calibration")))
      (if notice
        ($ :.form-notice notice)))))
