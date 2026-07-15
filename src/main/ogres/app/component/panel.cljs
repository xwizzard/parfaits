(ns ogres.app.component.panel
  (:require [ogres.app.component :refer [icon]]
            [ogres.app.component.panel-data :as data]
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

(def ^:private data
  {:data       {:icon "wrench-adjustable-circle" :label "Manage local data"}
   :initiative {:icon "hourglass-split" :label "Initiative"}
   :lobby      {:icon "people-fill" :label "Online options"}
   :scene      {:icon "easel" :label "Scene options"}
   :tokens     {:icon "person-circle" :label "Token images"}
   :props      {:icon "images" :label "Prop images"}
   :game-type-builder {:icon "sliders" :label "Game builder"}})

(def ^:private components
  {:data       {:form data/panel}
   :initiative {:form initiative/panel :footer initiative/actions}
   :lobby      {:form lobby/panel :footer lobby/actions}
   :scene      {:form scene/panel}
   :tokens     {:form tokens/panel :footer tokens/actions}
   :props      {:form props/panel :footer props/actions}
   :game-type-builder {:form game-type-builder/panel}})

(defn ^:private visible-tabs
  "The ordered list of visible panel tab keys for the given host status,
   interface mode, and the active scene's game-type enabled-elements set.
   Builder mode shows only the game-type editor, decoupled from any scene.
   Play mode drops the setup-only tabs. The Initiative tab additionally
   requires the active game-type to have :system/initiative-roll enabled,
   in either mode -- if a game type doesn't use that system at all, the
   tab shouldn't appear while setting up the scene either."
  [host mode enabled-elements]
  (cond
    (not host) [:tokens :initiative :lobby]
    (= mode :builder) [:game-type-builder]
    :else
    (cond-> (if (= mode :play)
              [:tokens :initiative :lobby]
              [:tokens :scene :props :initiative :lobby :data])
      (not (contains? enabled-elements :system/initiative-roll))
      (->> (remove #{:initiative}) vec))))

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
              ($ icon {:name (:icon data) :size 22}))))
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
