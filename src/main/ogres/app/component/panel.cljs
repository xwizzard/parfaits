(ns ogres.app.component.panel
  (:require [ogres.app.component :refer [icon]]
            [ogres.app.component.panel-data :as data]
            [ogres.app.component.panel-decks :as decks]
            [ogres.app.component.panel-game-type-builder :as game-type-builder]
            [ogres.app.component.panel-initiative :as initiative]
            [ogres.app.component.panel-lobby :as lobby]
            [ogres.app.component.panel-scene :as scene]
            [ogres.app.component.panel-tokens :as tokens]
            [ogres.app.component.panel-props :as props]
            [ogres.app.hooks :as hooks]
            [uix.core :refer [defui $]]))

(def ^:private query
  [[:user/host :default true]
   [:user/mode :default :setup]
   [:panel/selected :default :tokens]
   [:panel/expanded :default true]
   {:user/camera
    [{:camera/scene
      [{:scene/game-type
        [[:game-type/enabled-elements :default #{}]]}]}]}])

(def ^:private query-status
  [{:root/user [:user/host [:session/status :default :initial]]
    :root/session [:session/conns :session/room]}])

(def ^:private status-icon
  ($ icon {:name "globe-americas" :size 16}))

(defui status []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query-status [:db/ident :root])
        {{host :user/host status :session/status} :root/user
         {code  :session/room
          conns :session/conns} :root/session} result
        connected (cond-> (count conns) host (inc))]
    (case status
      :initial      ($ :button.button {:on-click #(dispatch :session/request)} status-icon "Start online game")
      :connecting   ($ :button.button {:disabled true} status-icon "Connecting...")
      :connected    ($ :button.button {:disabled true} status-icon "Connected / " code " / [ " connected " ]")
      :disconnected ($ :button.button {:disabled true} status-icon "Disconnected")
      ($ :button.button {:disabled true} status-icon "Status not known"))))

(def ^:private mode-options
  [[:builder "file-text" "Game Builder"]
   [:setup "easel" "Setup"]
   [:play "play-fill" "Play"]])

(defn ^:private next-mode
  "The mode that follows the given mode in the Builder -> Setup -> Play
   cycle used by the mode tab's single click-to-advance button."
  [mode]
  (case mode :builder :setup :setup :play :play :builder :setup))

(def ^:private data
  {:data       {:icon "floppy" :label "Manage local data" :size 26}
   :initiative {:icon "hourglass-split" :label "Turn Order"}
   :lobby      {:icon "people-fill" :label "Online options"}
   :scene      {:icon "images" :label "Scene options"}
   :tokens     {:icon "pawn" :label "Token images"}
   :props      {:icon "rock" :label "Prop images" :size 26}
   :decks      {:icon "suit-spade-fill" :label "Decks"}
   :game-type-builder {:icon "sliders" :label "Game builder"}})

(def ^:private components
  {:data       {:form data/panel}
   :initiative {:form initiative/panel :footer initiative/actions}
   :lobby      {:form lobby/panel :footer lobby/actions}
   :scene      {:form scene/panel}
   :tokens     {:form tokens/panel :footer tokens/actions}
   :props      {:form props/panel :footer props/actions}
   :decks      {:form decks/panel :footer decks/actions}
   :game-type-builder {:form game-type-builder/panel}})

(defn ^:private visible-tabs
  "The ordered list of visible panel tab keys for the given host status,
   interface mode, and the active scene's game-type enabled-elements set.
   Builder mode shows only the game-type editor and local data management,
   decoupled from any scene. Setup mode is scene-editing only. Play mode
   drops Scene (no more layout changes once play has started) but keeps
   Props (still placeable/adjustable mid-session) and is the only mode
   with the Turn Order tab and Lobby, since those are both live-session
   concerns. The Turn Order tab additionally requires the active
   game-type to have :unit/initiative enabled -- the base per-token
   'participates in the turn tracker' flag every game-type starts with
   (see ogres.app.game-type/default-enabled-elements), making turn
   tracking baseline rather than something only specific game modules
   (e.g. D&D 5e's d20 roll) unlock. The Decks tab, in both Setup and Play,
   likewise requires :tool/cards -- the generic card/deck system is opt-in
   the same way, not enabled by any seeded template yet."
  [host mode enabled-elements]
  (let [cards? (contains? enabled-elements :tool/cards)]
    (cond
      (not host) [:tokens :initiative :lobby]
      (= mode :builder) [:game-type-builder :data]
      (= mode :play)
      (into (cond-> [:tokens :props] cards? (conj :decks))
            (if (contains? enabled-elements :unit/initiative)
              [:initiative :lobby]
              [:lobby]))
      :else (cond-> [:scene :props :tokens] cards? (conj :decks)))))

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query query)
        {host :user/host
         mode :user/mode
         selected :panel/selected
         expanded :panel/expanded
         {{{enabled-elements :game-type/enabled-elements}
           :scene/game-type} :camera/scene} :user/camera} result
        tabs     (visible-tabs host mode enabled-elements)
        selected (if ((set tabs) selected) selected (first tabs))]
    ($ :.panel
      {:data-expanded expanded}
      (if expanded
        ($ :.panel-status
          ($ status)))
      ($ :ul.panel-tabs
        {:role "tablist"
         :aria-controls "form-panel"
         :aria-orientation "vertical"}
        (if host
          (let [[_ icon-name label] (some #(when (= (first %) mode) %) mode-options)]
            ($ :li.panel-tabs-mode
              {:role "tab"}
              ($ :button
                {:type "button"
                 :aria-label (str "Interface mode: " label ". Click to switch mode.")
                 :data-tooltip label
                 :on-click #(dispatch :user/change-mode (next-mode mode))}
                (if (= mode :builder)
                  ($ :span {:style {:display "inline-flex" :transform "translateY(3px) scale(1.15)"}}
                    ($ icon {:name icon-name :size 22}))
                  ($ icon {:name icon-name :size 22}))))))
        (for [[key data] (map (juxt identity data) tabs)
              :let [selected (= selected key)]]
          ($ :li.panel-tabs-tab
            {:key key :role "tab" :aria-selected (and expanded selected)}
            ($ :label {:aria-label (:label data)}
              ($ :input
                {:type "radio"
                 :name "panel"
                 :value key
                 :checked (and expanded selected)
                 :on-change #(dispatch :user/select-panel key)})
              ($ icon {:name (:icon data) :size (:size data 22)}))))
        ($ :li.panel-tabs-control
          {:role "tab" :on-click #(dispatch :user/toggle-panel)}
          ($ :button {:type "button" :aria-label "Collapse or expand"}
            ($ icon {:name (if expanded "chevron-double-right" "chevron-double-left")}))))
      (if expanded
        ($ :.form
          {:id "form-panel"
           :role "tabpanel"
           :data-form (name selected)}
          ($ :.form-container
            ($ :.form-content
              (if-let [component (get-in components [selected :form])]
                ($ :.form-body ($ component)))
              (if-let [component (get-in components [selected :footer])]
                ($ :.form-footer ($ component))))))))))
