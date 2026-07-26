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
     [:game-type/category :default nil]
     [:game-type/enabled-elements :default #{}]
     [:game-type/icon-overrides :default {}]]}
   {:root/user
    [{:user/game-type-editing [:db/id]}]}])

(def ^:private generic-flat-namespaces
  "The framework's own built-in elements (`ogres.app.game-type.core-
   elements`) are universal across every game and get grouped under one
   top-level 'Generic' category (see `editor`), sibling to each compiled-
   in game module's own section. These particular namespaces are listed
   directly under Generic with no further nesting -- only grid layouts
   (see `generic-nested-subcategory-*` below) get their own subsection,
   since there are enough of them to be worth collapsing separately."
  ["system" "tool" "unit"])

(def ^:private generic-nested-subcategory-labels
  {"grid" "Map tools"})

(def ^:private generic-nested-subcategory-order
  ["grid"])

(def ^:private grid-element-ids
  "The :tool/grid-* elements broken out of the Generic category's 'Map
   tools' subsection -- one entry per concrete :scene/grid-type value."
  (into #{} (filter #(re-find #"^grid-" (name %))) (keys game-type/elements)))

(defn ^:private element-label [id]
  (or (:label (get game-type/elements id))
      (-> id name capitalize)))

(defui ^:private element-row [{:keys [id game-type dispatch]}]
  (let [{editing-id :db/id
         enabled :game-type/enabled-elements
         overrides :game-type/icon-overrides} game-type
        {:keys [reserved? exclusive-group]} (get game-type/elements id)
        checked (contains? enabled id)
        ;; If this element loses an :exclusive-group tug-of-war (see
        ;; ogres.app.game-type/exclusive-group), name the element that's
        ;; currently holding that group so the tooltip explains why
        ;; checking this box will visibly uncheck another one.
        conflict (if (and exclusive-group (not checked))
                   (first (filter #(and (not= % id)
                                         (= (:exclusive-group (get game-type/elements %)) exclusive-group))
                                   enabled)))
        override (get overrides id)
        icon-name (or (:icon/sprite-name override) (:icon (get game-type/elements id)))]
    ($ :li.game-type-builder-element
      {:data-reserved reserved?}
      ($ :label.checkbox
        {:title (if conflict (str "Enabling this will disable " (element-label conflict)))}
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

(defui ^:private category-summary
  "A <summary> for a category/subcategory <details>, replacing the
   browser's default disclosure triangle with a directory-tree-style
   plus/minus marker (see panel_game_type_builder.css for the open/closed
   swap and the indentation that steps nested categories to the right).

   Also carries the category-level 'select all' checkbox -- checked when
   every id in `ids` is enabled, unchecked when none are, indeterminate
   for a partial mix (the same tri-state convention
   ogres.app.game-type.widgets/status-checklist uses for a multi-token
   selection, reimplemented locally here since this is a core file, not
   a leaf a game module could reach). Toggling it dispatches
   :game-type/toggle-category for the whole set in one transaction --
   this is what makes 'enable/disable all D&D 5e features' or 'all
   Gloomhaven features' a single click instead of one per element.

   `disable-ids`, when given, are force-disabled in that same
   transaction on enable only (see :game-type/toggle-category) -- used
   for a module tied to one specific map/grid type (see
   `ogres.app.game-type/category-grid-elements`), so enabling its
   category also clears every other grid layout instead of just adding
   its preferred one alongside them."
  [{:keys [label ids disable-ids game-type dispatch] :or {disable-ids #{}}}]
  (let [{editing-id :db/id enabled :game-type/enabled-elements} game-type
        checked-count (count (filter enabled ids))
        state (cond (zero? checked-count) false
                    (= checked-count (count ids)) true
                    :else :indeterminate)
        input (uix/use-ref)]
    (uix/use-effect
     (fn [] (if-let [node @input] (set! (.-indeterminate node) (= state :indeterminate))))
     [state])
    ($ :summary.game-type-builder-category-summary
      ($ :span.game-type-builder-category-marker-closed ($ icon {:name "plus" :size 12}))
      ($ :span.game-type-builder-category-marker-open ($ icon {:name "dash" :size 12}))
      ($ :label.checkbox.game-type-builder-category-checkbox
        ;; Stop the click here so it doesn't also toggle the parent
        ;; <details> open/closed -- a native <summary> click's default
        ;; action (the disclosure toggle) fires for any click inside it,
        ;; including this checkbox, unless propagation is cut off before
        ;; it reaches <details>'s own handling.
        {:on-click (fn [event] (.stopPropagation event))}
        ($ :input
          {:type "checkbox"
           :ref input
           :checked (true? state)
           :on-change
           (fn [event]
             (let [checked (.. event -target -checked)]
               (if checked
                 (dispatch :game-type/toggle-category editing-id (set ids) true (set disable-ids))
                 (dispatch :game-type/toggle-category editing-id (set ids) false))))})
        ($ icon {:name "check" :size 16}))
      label)))

(def ^:private known-categories
  "Namespace segments claimed by the Generic category above (flat or
   nested) -- anything else present in the registry is a compiled-in game
   module's own elements (e.g. \"dnd5e\"/\"gloomhaven\") and gets
   rendered as its own top-level section after Generic."
  (into (set generic-flat-namespaces) generic-nested-subcategory-order))

(defui ^:private editor [{:keys [game-type dispatch]}]
  (let [id (:db/id game-type)
        all-ids (sort (keys game-type/elements))
        by-category (-> (group-by namespace (remove grid-element-ids all-ids))
                         (assoc "grid" (filter grid-element-ids all-ids)))
        ;; Every namespace not claimed by the Generic category above --
        ;; one section per game module, titled via `game-type/game-label`.
        ;; A new game module needs zero edits here.
        game-categories (sort (remove known-categories (keys by-category)))
        generic-flat-ids (into [] (mapcat by-category) generic-flat-namespaces)
        ;; Generic's own "select all" checkbox spans everything under it,
        ;; flat items and the nested Map tools subcategory alike.
        generic-ids (into generic-flat-ids (get by-category "grid"))]
    ($ :<>
      ($ :fieldset.fieldset
        ($ :legend "Name")
        ($ :input.text.text-ghost
          {:type "text"
           :maxLength 36
           :value (or (:game-type/name game-type) "")
           :on-change #(dispatch :game-type/rename id (.. % -target -value))}))
      ($ :details.game-type-builder-category
        ($ category-summary
          {:label "Generic" :ids generic-ids :game-type game-type :dispatch dispatch})
        ;; Subcategories (currently just Map tools) render before the
        ;; flat item list, so nested groups always sort to the top of
        ;; their parent -- matching a directory tree, where folders list
        ;; above loose files.
        (for [subcategory generic-nested-subcategory-order
              :let [ids (get by-category subcategory)]
              :when (seq ids)]
          ($ :details.game-type-builder-category {:key subcategory}
            ($ category-summary
              {:label (get generic-nested-subcategory-labels subcategory)
               :ids ids :game-type game-type :dispatch dispatch})
            ($ element-list {:ids ids :game-type game-type :dispatch dispatch})))
        ($ element-list {:ids generic-flat-ids :game-type game-type :dispatch dispatch}))
      (for [category game-categories
            :let [ids (get by-category category)
                  ;; A module tied to one specific map/grid type (see
                  ;; game-type/category-grid-elements, e.g. Gloomhaven's
                  ;; always-hex-pointy board) gets its preferred grid
                  ;; element folded into this checkbox's own scope, and
                  ;; every *other* grid element queued to force off when
                  ;; enabling -- a module not listed there is unaffected,
                  ;; a plain category toggle with no grid side effects.
                  grid-preference (get game-type/category-grid-elements category)
                  category-ids (if grid-preference (into (set ids) grid-preference) ids)
                  ;; A module can also exclude non-grid ids it has no
                  ;; mechanic for at all (see
                  ;; game-type/category-excluded-elements, e.g.
                  ;; Gloomhaven having no light-radius mechanic).
                  disable-ids (into (set (if grid-preference (remove grid-preference grid-element-ids) []))
                                     (get game-type/category-excluded-elements category))]
            :when (seq ids)]
        ($ :details.game-type-builder-category {:key category}
          ($ category-summary
            {:label (game-type/game-label category)
             :ids category-ids
             :disable-ids disable-ids
             :game-type game-type
             :dispatch dispatch})
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

(defui ^:private template-row [{:keys [game-type active removable? dispatch]}]
  (let [checked (= (:db/id game-type) (:db/id active))]
    ($ :.game-type-builder-template
      ($ :label.radio
        ($ :input
          {:type "radio"
           :name "game-type-editing"
           :checked checked
           :on-change #(dispatch :user/edit-game-type (:db/id game-type))})
        (:game-type/name game-type))
      (if removable?
        ($ :button.game-type-builder-remove
          {:type "button"
           :aria-label (str "Remove " (:game-type/name game-type))
           :on-click #(dispatch :game-type/remove (:db/id game-type))}
          ($ icon {:name "trash3-fill" :size 16}))))))

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
        (let [by-category (group-by :game-type/category game-types)
              ungrouped (get by-category nil)
              categories (sort (remove nil? (keys by-category)))]
          ($ :<>
            ($ :.input-group
              (for [gt ungrouped]
                ($ template-row {:key (:db/id gt) :game-type gt :active active
                                  :removable? removable? :dispatch dispatch})))
            (for [category categories
                  :let [templates (get by-category category)]]
              ($ :.game-type-builder-template-group {:key category}
                ($ :.game-type-builder-template-group-label (game-type/game-label category))
                ($ :.input-group
                  (for [gt templates]
                    ($ template-row {:key (:db/id gt) :game-type gt :active active
                                      :removable? removable? :dispatch dispatch})))))))
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
