(ns ogres.app.component.panel-library-browser
  "The unified image library browser -- opened from each gallery panel
   (Tokens/Props/Scene), pre-scoped to that gallery's own partition of
   :root/library. Absorbs what used to be scattered across each
   panel's own actions area: Upload, Link (by URL), crop-Edit (tokens),
   category/folder organization, and deleting an image from the
   archive outright. Placement (dragging a thumbnail onto the canvas)
   stays in each panel's own small grid, unchanged -- this dialog is
   only ever about acquiring/organizing/forgetting images, never about
   placing them.

   :root/library membership and every image's calibration both live
   directly on the shared [:image/hash h] entity (see events.cljs's
   reference-counted remove logic and provider/state.cljs's :root/
   library schema entry) -- so 'active in this gallery' is just
   whether the same hash also appears in :root/token-images et al,
   checked here by set membership rather than a second round-trip.

   `render-editor`, an optional {:keys [gallery on-close render-editor]}
   prop, is `(fn [entry on-done])` -> element, rendered in place of the
   grid when an entry's own Edit button is clicked. Deliberately a
   caller-supplied render-prop rather than this namespace reaching into
   panel_tokens.cljs's crop editor directly -- panel_tokens.cljs already
   requires this namespace for its own 'Browse Library' button, and a
   direct require the other way would be circular. Only the Tokens
   panel supplies one today (its existing hash-only crop editor,
   panel_tokens.cljs's `editor`); Props/Scene have no hash-only
   calibration editor yet (see the plan's explicitly-out-of-scope
   note), so their entries simply have no Edit button."
  (:require [clojure.string :as string]
            [ogres.app.component :as component :refer [icon]]
            [ogres.app.hooks :as hooks]
            [uix.core :as uix :refer [defui $]]))

(def ^:private gallery-root-attr
  {:token :root/token-images :props :root/props-images :scene :root/scene-images})

(def ^:private gallery-label
  {:token "Token" :props "Props" :scene "Scene"})

(def ^:private query
  [{:root/user [:user/host]}
   {:root/library
    [:image/hash :image/name :image/width :image/height :image/size
     :image/cell-px :image/rotation :image/anchor
     :image/thumbnail-rect :image/thumbnail-rotation
     :library/gallery :library/category :library/added-at
     {:image/thumbnail [:image/hash]}]}
   {:root/token-images [:image/hash]}
   {:root/props-images [:image/hash]}
   {:root/scene-images [:image/hash]}])

(defn ^:private remove-from-gallery!
  "The three gallery-remove events don't share one signature
   (:props-images/remove never grew a thumbnail-hash argument the way
   scene/token did, back when that mattered -- see events.cljs) --
   this just hides that irregularity from the browser UI."
  [dispatch gallery hash]
  (case gallery
    :token (dispatch :token-images/remove hash hash)
    :props (dispatch :props-images/remove hash)
    :scene (dispatch :scene-images/remove hash hash)))

(defui ^:private library-entry [{:keys [entry gallery active? host dispatch on-edit render-editor]}]
  (let [{hash :image/hash name :image/name category :library/category} entry
        thumb (:image/hash (:image/thumbnail entry))
        url (hooks/use-image thumb)]
    ($ :.library-entry
      ($ :.library-entry-thumb {:style {:background-image (str "url(" url ")")}})
      ($ :.library-entry-name {:title name} (or name hash))
      ($ :label.checkbox.library-entry-active
        ($ :input
          {:type "checkbox"
           :checked (boolean active?)
           :on-change
           (fn []
             (if active?
               (remove-from-gallery! dispatch gallery hash)
               (dispatch :library/add-to-gallery hash gallery)))})
        ($ icon {:name "check" :size 16})
        (if active? "Active in gallery" "Add to gallery"))
      ($ :input.text.text-ghost.library-entry-category
        {:type "text"
         :placeholder "Uncategorized"
         :default-value category
         :on-blur (fn [e] (dispatch :library/set-category hash (.. e -target -value)))})
      (if render-editor
        ($ :button.button.button-neutral
          {:type "button" :on-click #(on-edit entry)}
          ($ icon {:name "crop" :size 16}) "Edit"))
      (if host
        ($ :button.button.button-danger
          {:type "button"
           :aria-label (str "Delete " (or name hash) " from the library")
           :on-click
           (fn []
             (if (js/confirm (str "Delete \"" (or name hash) "\" from the library? "
                                   "This removes it from every gallery and cannot be undone."))
               (dispatch :library/remove-image hash)))}
          ($ icon {:name "trash3-fill" :size 16}))))))

(defui ^:private category-group [{:keys [category entries gallery active-hashes host dispatch on-edit render-editor]}]
  ($ :details.library-browser-category {:key (or category "uncategorized") :open true}
    ($ :summary (or category "Uncategorized"))
    ($ :.library-browser-grid
      (for [entry (sort-by (comp - (fnil identity 0) :library/added-at) entries)]
        ($ library-entry
          {:key (:image/hash entry)
           :entry entry
           :gallery gallery
           :active? (contains? active-hashes (:image/hash entry))
           :host host
           :dispatch dispatch
           :on-edit on-edit
           :render-editor render-editor})))))

(defui panel-library-browser [{:keys [gallery on-close render-editor]}]
  (let [dispatch (hooks/use-dispatch)
        upload (hooks/use-image-uploader {:type gallery})
        input (uix/use-ref)
        [filter-text set-filter-text] (uix/use-state "")
        [editing set-editing] (uix/use-state nil)
        result (hooks/use-query query [:db/ident :root])
        {host :user/host} (:root/user result)
        active-hashes (into #{} (map :image/hash) (get result (gallery-root-attr gallery)))
        entries (filter (comp #{gallery} :library/gallery) (:root/library result))
        needle (string/lower-case (string/trim filter-text))
        matches? (fn [{:keys [image/name library/category]}]
                   (or (string/blank? needle)
                       (string/includes? (string/lower-case (or name "")) needle)
                       (string/includes? (string/lower-case (or category "")) needle)))
        visible (filter matches? entries)
        by-category (group-by :library/category visible)
        categories (sort (remove nil? (keys by-category)))]
    ($ component/fullscreen-dialog
      {:on-close on-close}
      ($ :.library-browser
        ($ :header ($ :h2 (str (gallery-label gallery) " Library")))
        (if editing
          ($ :.library-browser-editor
            (render-editor editing #(set-editing nil))
            ($ :button.button.button-neutral {:type "button" :on-click #(set-editing nil)} "Back"))
          ($ :<>
            ($ :.library-browser-controls
              ($ :input
                {:type "file" :hidden true :accept "image/*" :multiple true :ref input
                 :on-change
                 (fn [event]
                   (upload (.. event -target -files))
                   (set! (.. event -target -value) ""))})
              ($ :button.button.button-neutral
                {:type "button" :on-click #(.click (deref input))}
                ($ icon {:name "camera-fill" :size 16}) "Upload")
              ($ component/image-url-form {:type gallery})
              ($ :input.text.text-ghost.library-browser-filter
                {:type "search"
                 :placeholder "Filter by name or category…"
                 :value filter-text
                 :on-change #(set-filter-text (.. % -target -value))}))
            (if (empty? visible)
              ($ :.form-notice
                (if (seq entries)
                  "No images match that filter."
                  "Nothing in this library yet -- upload or link an image above."))
              ($ :<>
                (for [category categories]
                  ($ category-group
                    {:key category :category category :entries (get by-category category)
                     :gallery gallery :active-hashes active-hashes :host host
                     :dispatch dispatch :on-edit set-editing :render-editor render-editor}))
                (if-let [uncategorized (get by-category nil)]
                  ($ category-group
                    {:key "uncategorized" :category nil :entries uncategorized
                     :gallery gallery :active-hashes active-hashes :host host
                     :dispatch dispatch :on-edit set-editing :render-editor render-editor}))))))))))
