(ns ogres.app.component.panel-roster
  "The Players/NPCs roster panel/tab -- create, rename, recolor,
   bench/restore, and permanently remove persistent participants a card
   (or, later, any other asset) can be assigned to. Named `panel-roster`
   (not `panel-players`) deliberately, to avoid confusion with the
   existing, unrelated ogres.app.component.players (the always-on
   canvas-corner presence overlay for connected human sessions -- a
   different feature this namespace doesn't touch).

   The roster is global (:root/players) -- one shared list persisting
   across every scene, not owned by any single scene.

   Deliberately scoped to the roster itself: no card-hand or token-
   ownership assignment yet -- see ogres.app.player and events.cljs's
   :player/* methods for what's already generic and ready for that."
  (:require [ogres.app.component :refer [icon]]
            [ogres.app.hooks :as hooks]
            [ogres.app.player :as player]
            [uix.core :as uix :refer [defui $]]))

(def ^:private query
  [{:root/players
    [:db/id :player/name :player/kind :player/color [:player/active :default true]
     {:player/controller [:user/uuid]}]}
   {:root/session
    [{:session/conns [:db/id :user/uuid :user/color :user/label]}]}])

(def ^:private kind-labels
  {:human "Player" :npc "NPC"})

(defn ^:private other-kind
  [kind]
  (if (= kind :npc) :human :npc))

(defui ^:private color-picker
  "A color-swatch button that opens a small popover of the palette
   colors for this row's kind (see ogres.app.player/colors-for-kind --
   humans and NPCs draw from two disjoint palettes) as exclusive radios
   -- colors already held by a *different* participant of the same kind
   render disabled, so two participants of the same kind can never end up
   sharing one at the UI level (the :player/change-color event also
   enforces this itself, see its docstring)."
  [{:keys [current kind taken dispatch player-id]}]
  (let [[open? set-open form] (hooks/use-modal)]
    ($ :.roster-color-wrapper
      ($ :button.roster-color-trigger
        {:type "button"
         :data-color current
         :aria-label "Change color"
         :title "Change color"
         :on-click
         (fn [event]
           (.stopPropagation event)
           (set-open not))})
      (if open?
        ($ :.roster-color-form
          {:ref form}
          (for [[value label] (player/colors-for-kind kind)]
            ($ :label.roster-color-swatch
              {:key value :data-color value :aria-label label :title label}
              ($ :input
                {:type "radio"
                 :name (str "roster-color-" player-id)
                 :checked (= value current)
                 :disabled (and (not= value current) (contains? taken value))
                 :on-change
                 (fn []
                   (dispatch :player/change-color player-id value)
                   (set-open false))}))))))))

(defn ^:private conn-label
  [conn]
  (or (:user/label conn) (str "Guest (" (:user/color conn) ")")))

(defui ^:private controller-picker
  "A <select> assigning which currently-connected guest controls this
   roster player -- \"Host\" (the default, clears :player/controller) or
   one of the currently-connected guests (see events.cljs's
   :player/set-controller). Value is the controller's :user/uuid, or ''
   for Host; the guest's own :db/id is resolved by the event via a
   [:user/uuid ...] lookup ref rather than passed directly, so this
   component never needs to know :db/id at all."
  [{:keys [controller-uuid conns dispatch player-id]}]
  ($ :select.roster-row-controller
    {:value (or controller-uuid "")
     :on-change
     (fn [event]
       (let [v (.. event -target -value)]
         (dispatch :player/set-controller player-id (if (seq v) [:user/uuid v] nil))))}
    ($ :option {:value ""} "Host")
    (for [conn conns]
      ($ :option {:key (:user/uuid conn) :value (:user/uuid conn)} (conn-label conn)))))

(defui ^:private roster-row [{:keys [entity dispatch taken conns]}]
  (let [{id :db/id name :player/name kind :player/kind
         color :player/color active :player/active} entity
        controller-uuid (get-in entity [:player/controller :user/uuid])]
    ($ :li.roster-row
      {:data-active active}
      ($ color-picker
        {:current color :kind kind :dispatch dispatch :player-id id
         ;; A player's own current color shouldn't count as "taken" from
         ;; its own picker's point of view -- otherwise it would show as
         ;; disabled the moment you open your own swatch. Human and NPC
         ;; color keys never overlap, so scoping `taken` per-kind here
         ;; isn't required for correctness, but the popover only ever
         ;; shows this row's own kind's palette regardless.
         :taken (disj taken color)})
      ($ :input.text.text-ghost.roster-row-name
        {:type "text"
         :key (str "name:" name)
         :default-value name
         :placeholder "Name"
         :max-length 32
         :on-blur (fn [event] (dispatch :player/rename id (.. event -target -value)))})
      ($ :button.button.button-neutral.roster-row-kind
        {:type "button"
         :aria-label (str "Kind: " (kind-labels kind) ". Click to change.")
         :title "Click to change kind"
         :on-click #(dispatch :player/change-kind id (other-kind kind))}
        (kind-labels kind))
      ($ controller-picker
        {:controller-uuid controller-uuid :conns conns :dispatch dispatch :player-id id})
      ($ :button.roster-row-toggle
        {:type "button"
         :data-active active
         :aria-label (if active "Remove from active play" "Add to active play")
         :title (if active "Remove from active play" "Add to active play")
         :on-click #(dispatch :player/set-active id (not active))}
        ($ :.roster-row-toggle-knob
          ($ icon {:name (if active "check" "x") :size 12})))
      ($ :button.button.button-danger
        {:type "button" :aria-label "Remove permanently" :title "Remove permanently"
         :on-click #(dispatch :player/remove id)}
        ($ icon {:name "trash3-fill" :size 16})))))

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        entities (:root/players result)
        conns (:session/conns (:root/session result))
        taken (into #{} (map :player/color) entities)]
    ($ :.form-roster
      ($ :header ($ :h2 "Players"))
      (if (seq entities)
        ($ :ul.roster-list
          (for [entity entities]
            ($ roster-row {:key (:db/id entity) :entity entity :dispatch dispatch :taken taken :conns conns})))
        ($ :.form-notice
          "Add players and NPCs below. Each gets its own color, useful for
           telling participants apart at a glance and, soon, for assigning
           cards or other items to them.")))))

(defui ^:memo actions []
  (let [dispatch (hooks/use-dispatch)]
    ($ :<>
      ($ :button.button.button-neutral
        {:type "button" :on-click #(dispatch :player/create :human)}
        ($ icon {:name "person-circle" :size 16})
        "Add Player")
      ($ :button.button.button-neutral
        {:type "button" :on-click #(dispatch :player/create :npc)}
        ($ icon {:name "person-circle" :size 16})
        "Add NPC"))))
