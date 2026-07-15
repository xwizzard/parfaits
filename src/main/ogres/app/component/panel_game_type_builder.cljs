(ns ogres.app.component.panel-game-type-builder
  "Game Builder mode: create/select/rename a game-type template and toggle
   which registry elements (per-unit fields, gameplay tools, gameplay-
   specific systems) it enables. Game types are isolated to this mode --
   selecting or creating a template here both opens it for editing and
   makes it the active game-type for the current scene; there is no
   separate scene-level picker."
  (:require [cljs.reader :refer [read-string]]
            [clojure.string :refer [capitalize]]
            [ogres.app.component :refer [icon]]
            [ogres.app.game-type :as game-type]
            [ogres.app.hooks :as hooks]
            [ogres.app.util :as util]
            [uix.core :as uix :refer [defui $]]))

(def ^:private query
  [{:root/game-types
    [:db/id
     :game-type/name
     [:game-type/enabled-elements :default #{}]
     [:game-type/icon-overrides :default {}]]}
   {:root/user
    [{:user/game-type-editing [:db/id]}]}])

(def ^:private category-labels
  {"unit" "Per-unit elements"
   "grid" "Map tools"})

(def ^:private category-order
  ["grid" "unit"])

(def ^:private gameplay-subcategory-labels
  {"system" "Systems"
   "tool" "Tools"})

(def ^:private gameplay-subcategory-order
  ["system" "tool"])

(def ^:private grid-element-ids
  "The :tool/grid-* elements broken out of Gameplay tools into their own
   top-level 'Map tools' category -- one entry per concrete :scene/grid-type
   value."
  (into #{} (filter #(re-find #"^grid-" (name %))) (keys game-type/elements)))

(defn ^:private element-label [id]
  (or (:label (get game-type/elements id))
      (-> id name capitalize)))

(defui ^:private element-row [{:keys [id game-type dispatch]}]
  (let [{editing-id :db/id
         enabled :game-type/enabled-elements
         overrides :game-type/icon-overrides} game-type
        {:keys [reserved?]} (get game-type/elements id)
        checked (contains? enabled id)
        override (get overrides id)
        icon-name (or (:icon/sprite-name override) (:icon (get game-type/elements id)))]
    ($ :li.game-type-builder-element
      {:data-reserved reserved?}
      ($ :label.checkbox
        ($ :input
          {:type "checkbox"
           :disabled reserved?
           :checked checked
           :on-change
           (fn [event]
             (dispatch :game-type/toggle-element editing-id id (.. event -target -checked)))})
        ($ icon {:name "check" :size 20})
        (if icon-name ($ icon {:name icon-name :size 16}))
        (element-label id)
        (if reserved? ($ :span.game-type-builder-reserved "(reserved)")))
      ($ :input.text.text-ghost
        {:type "text"
         :placeholder "Icon URL override"
         :value (or (:icon/url override) "")
         :on-change
         (fn [event]
           (let [value (.. event -target -value)]
             (dispatch :game-type/set-icon-override editing-id id
                       (if (= value "") nil {:icon/url value}))))}))))

(defui ^:private element-list [{:keys [ids game-type dispatch]}]
  ($ :ul.game-type-builder-elements
    (for [element-id ids]
      ($ element-row {:key element-id :id element-id :game-type game-type :dispatch dispatch}))))

(defui ^:private editor [{:keys [game-type dispatch]}]
  (let [id (:db/id game-type)
        all-ids (sort (keys game-type/elements))
        by-category (-> (group-by namespace (remove grid-element-ids all-ids))
                         (assoc "grid" (filter grid-element-ids all-ids)))]
    ($ :<>
      ($ :fieldset.fieldset
        ($ :legend "Name")
        ($ :input.text.text-ghost
          {:type "text"
           :maxLength 36
           :value (or (:game-type/name game-type) "")
           :on-change #(dispatch :game-type/rename id (.. % -target -value))}))
      ($ :details.game-type-builder-category
        ($ :summary "Gameplay")
        (for [subcategory gameplay-subcategory-order
              :let [ids (get by-category subcategory)]
              :when (seq ids)]
          ($ :details.game-type-builder-category {:key subcategory}
            ($ :summary (get gameplay-subcategory-labels subcategory))
            ($ element-list {:ids ids :game-type game-type :dispatch dispatch}))))
      (for [category category-order
            :let [ids (get by-category category)]
            :when (seq ids)]
        ($ :details.game-type-builder-category {:key category}
          ($ :summary (get category-labels category category))
          ($ element-list {:ids ids :game-type game-type :dispatch dispatch}))))))

(defn ^:private export-game-type! [game-type]
  (let [data {:name (:game-type/name game-type)
              :enabled-elements (:game-type/enabled-elements game-type)
              :icon-overrides (:game-type/icon-overrides game-type)}
        blob (js/Blob. #js [(pr-str data)] #js {"type" "application/edn"})]
    (util/download blob (str (:game-type/name game-type) ".gametype.edn"))))

(defn ^:private import-game-type! [dispatch file]
  ;; :game-type/import sanitizes elements/overrides itself (the file's
  ;; contents are untrusted); this only needs to parse the EDN and supply
  ;; a fallback name.
  (-> (.text file)
      (.then
       (fn [text]
         (try
           (let [data (read-string text)]
             (dispatch :game-type/import
                       {:name (:name data "Imported game type")
                        :enabled-elements (:enabled-elements data)
                        :icon-overrides (:icon-overrides data)}))
           (catch :default e
             (js/console.error "Failed to import game type:" e)))))))

(defui panel []
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query query [:db/ident :root])
        import-input (uix/use-ref)
        {game-types :root/game-types
         {editing :user/game-type-editing} :root/user} result
        editing-id (:db/id editing)
        active (or (first (filter (comp #{editing-id} :db/id) game-types))
                   (first game-types))
        removable? (> (count game-types) 1)]
    ($ :form.form-game-type-builder
      {:on-submit (fn [event] (.preventDefault event))}
      ($ :header ($ :h2 "Game builder"))
      ($ :fieldset.fieldset
        ($ :legend "Templates")
        ($ :.input-group
          (for [gt game-types
                :let [checked (= (:db/id gt) (:db/id active))]]
            ($ :.game-type-builder-template {:key (:db/id gt)}
              ($ :label.radio
                ($ :input
                  {:type "radio"
                   :name "game-type-editing"
                   :checked checked
                   :on-change #(dispatch :user/edit-game-type (:db/id gt))})
                (:game-type/name gt))
              (if removable?
                ($ :button.game-type-builder-remove
                  {:type "button"
                   :aria-label (str "Remove " (:game-type/name gt))
                   :on-click #(dispatch :game-type/remove (:db/id gt))}
                  ($ icon {:name "trash3-fill" :size 16}))))))
        ($ :.input-group
          ($ :button.button.button-neutral
            {:type "button"
             :on-click #(dispatch :game-type/create (:db/id active) "New game type")}
            ($ icon {:name "plus" :size 16}) "New template")
          ($ :button.button.button-neutral
            {:type "button"
             :disabled (nil? active)
             :on-click #(export-game-type! active)}
            ($ icon {:name "box-arrow-up-right" :size 16}) "Export")
          ($ :button.button.button-neutral
            {:type "button"
             :on-click #(.click (deref import-input))}
            ($ icon {:name "door-open" :size 16}) "Import"
            ($ :input
              {:ref import-input
               :type "file"
               :hidden true
               :accept ".edn,text/plain"
               :on-change
               (fn [event]
                 (if-let [file (first (array-seq (.. event -target -files)))]
                   (import-game-type! dispatch file))
                 (set! (.. event -target -value) ""))}))))
      (if active
        ($ editor {:game-type active :dispatch dispatch})))))
