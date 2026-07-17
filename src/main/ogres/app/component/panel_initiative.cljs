(ns ogres.app.component.panel-initiative
  (:require [clojure.string :refer [join capitalize blank?]]
            [ogres.app.component :refer [icon image]]
            [ogres.app.game-type :as game-type]
            [ogres.app.hooks :as hooks]
            [uix.core :as uix :refer [defui $]]))

(def ^:private query
  [:user/host
   {:user/camera
    [{:camera/scene
      [:db/id
       :initiative/rounds
       :initiative/turn
       :initiative/played
       {:scene/game-type
        [[:game-type/enabled-elements :default #{}]]}
       {:scene/initiative
        [:db/id
         :object/hidden
         :token/label
         :token/flags
         :initiative/rank
         :initiative/suffix
         :initiative/health
         :camera/_selected
         {:token/image
          [:token-image/url
           :image/public
           {:image/thumbnail
            [:image/hash]}]}]}]}]}])

(def ^:private query-actions
  [{:user/camera
    [{:camera/scene
      [{:scene/initiative
        [:db/id :initiative/rank :token/flags]}
       [:initiative/rounds :default 0]
       :initiative/played
       {:scene/game-type
        [[:game-type/enabled-elements :default #{}]]}]}]}])

(defn ^:private initiative-order
  "Descending sort by turn-order rank, :db/id as a total-order tiebreak --
   see the identical comparator in ogres.app.events for the full rationale
   (kept as a separate copy here rather than shared, matching how the two
   files were already independent before this rename)."
  [a b]
  (let [f (juxt :initiative/rank :db/id)]
    (compare (f b) (f a))))

(defui ^:private rank-widget
  "The base turn-order control every game-type gets, regardless of
   whether any module contributes its own way of assigning order (e.g.
   D&D's dice-roll trigger, added alongside this via the generic
   :initiative-panel loop in `token` below). Click the rank to type an
   exact number, or use the up/down buttons to nudge this token earlier
   or later in the order -- both write the same generic
   :initiative/rank value, so this alone is enough to run a full game
   with manually-assigned, non-random turn order."
  [{:keys [value on-change on-move]}]
  (let [[editing set-editing form] (hooks/use-modal)
        input (uix/use-ref)]
    ($ :.initiative-token-roll
      {:data-present (some? value)}
      ($ :button.initiative-token-rank-move
        {:type "button"
         :aria-label "Move earlier in turn order"
         :on-click (fn [event] (.stopPropagation event) (on-move :earlier))}
        ($ icon {:name "arrow-up-short" :size 12}))
      ($ :button.initiative-token-roll-control
        {:on-click
         (fn [event]
           (.stopPropagation event)
           (set-editing not)
           (.requestAnimationFrame
            js/window
            #(if-let [node (deref input)]
               (.select node))))}
        (or value \-))
      ($ :button.initiative-token-rank-move
        {:type "button"
         :aria-label "Move later in turn order"
         :on-click (fn [event] (.stopPropagation event) (on-move :later))}
        ($ icon {:name "arrow-down-short" :size 12}))
      (if editing
        ($ :form.initiative-token-form
          {:ref form
           :data-type "rank"
           :on-submit
           (fn [event]
             (.preventDefault event)
             (on-change (.-value (deref input)))
             (set-editing not))}
          ($ :input.text.text-ghost
            {:type "number"
             :ref input
             :auto-focus true
             :default-value value
             :placeholder "Rank"
             :aria-label "Turn order"})
          ($ :button {:type "submit"}
            ($ icon {:name "check"})))))))

(defui ^:private token
  [{:keys [context entity]}]
  (let [dispatch (hooks/use-dispatch)
        {host :user/host
         {{curr :initiative/turn
           rnds :initiative/rounds
           went :initiative/played
           game-type-entity :scene/game-type}
          :camera/scene} :user/camera} context
        enabled-elements (:game-type/enabled-elements game-type-entity #{})
        {id :db/id
         label :token/label
         flags :token/flags
         suffix :initiative/suffix
         {{hash :image/hash} :image/thumbnail} :token/image} entity
        playing (= (:db/id curr) (:db/id entity))
        played (boolean (some #{{:db/id id}} went))
        hidden (and (not host) (:object/hidden entity))]
    ($ :li.initiative-token
      {:data-playing playing
       :data-played played
       :data-hidden hidden
       :data-type "token"}
      ($ :button.initiative-token-turn
        {:disabled (or (nil? rnds) (zero? rnds))
         :on-click
         (fn []
           (if played
             (dispatch :initiative/unmark id)
             (dispatch :initiative/mark id)))}
        ($ icon {:name "arrow-right-short"}))
      ($ rank-widget
        {:value (:initiative/rank entity)
         :on-change
         (fn [value]
           (dispatch :initiative/change-rank id value))
         :on-move
         (fn [direction]
           (dispatch :initiative/move id direction))})
      ($ :.initiative-token-frame
        {:on-click #(dispatch :objects/select id)
         :data-player (contains? flags :player)
         :data-hidden hidden}
        (cond hidden \?
              (some? hash)
              ($ image {:hash hash}
                (fn [url]
                  ($ :.initiative-token-image
                    {:style {:background-image (str "url(" url ")")}})))
              :else
              ($ :.initiative-token-pattern
                ($ icon {:name "dnd" :size 36}))))
      (if suffix
        ($ :.initiative-token-suffix (char (+ suffix 64))))
      ($ :.initiative-token-info
        (if (not (blank? label))
          ($ :.initiative-token-label label))
        ($ :.initiative-token-flags
          (if (seq flags)
            (join ", " (mapv (comp capitalize name) flags))))
        (if-let [url (:token-image/url (:token/image entity))]
          (if (or host (:image/public (:token/image entity)))
            (if-let [url (js/URL.parse url)]
              ($ :a.initiative-token-url
                {:href (.-href url) :target "_blank"}
                (str (.-hostname url)
                     (if (not= (.-pathname url) "/")
                       (.-pathname url))))
              ($ :.initiative-token-url url)))))
      ;; Any enabled element that declares an :initiative-panel gets its
      ;; own contribution rendered here (e.g. dnd5e/gloomhaven's HP
      ;; tracker) -- this file never names a specific game or mechanic,
      ;; it only looks for the presence of that key. Visibility is still
      ;; gated by host/player-flag here, centrally, rather than in each
      ;; game module, since that's a permission rule, not a game rule.
      (if (or host (contains? flags :player))
        (for [[element-id element] (filter (comp :initiative-panel val)
                                            (select-keys game-type/elements enabled-elements))]
          ($ :<> {:key element-id}
            ((get-in element [:initiative-panel :render]) {:entity entity :dispatch dispatch})))))))

(defui ^:private token-placeholder []
  ($ :li.initiative-token {:data-type "placeholder"}
    ($ :.initiative-token-turn
      ($ icon {:name "arrow-right-short"}))
    ($ :.initiative-token-roll
      ($ :.initiative-token-rank-move)
      ($ :.initiative-token-roll-control)
      ($ :.initiative-token-rank-move))
    ($ :.initiative-token-frame
      ($ :.initiative-token-pattern))
    ($ :.initiative-token-info)
    ($ :.initiative-token-health
      ($ :.initiative-token-health-frame
        ($ icon {:name "heart-fill" :size 40}))
      ($ :.initiative-token-health-label))))

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query query)
        {{{tokens :scene/initiative
           rounds :initiative/rounds} :camera/scene}
         :user/camera} result]
    ($ :.initiative
      ($ :header
        ($ :h2 "Turn Order")
        (if (>= rounds 1)
          ($ :h3 "Round " rounds)))
      (cond (and (not (seq tokens)) (nil? rounds))
            ($ :ol.initiative-list.initiative-list-placeholder
              (for [indx (range 6)]
                (if (= indx 1)
                  ($ :.initiative-prompt {:key indx :style {:text-align "center"}}
                    "Begin the turn order by selecting one or more tokens
                     and clicking the hourglass button.")
                  ($ token-placeholder {:key indx}))))
            (and (not (seq tokens)) (>= rounds 1))
            ($ :.prompt
              ($ :br)
              "The round is still running but there are no tokens participating."
              ($ :br)
              ($ :br)
              ($ :button.button.button-neutral
                {:on-click #(dispatch :initiative/leave)} "Leave"))
            (seq tokens)
            ($ :ol.initiative-list
              (for [entity (sort initiative-order tokens)]
                ($ token {:key (:db/id entity) :entity entity :context result})))))))

(defui ^:memo actions []
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query query-actions)
        {{{rounds :initiative/rounds
           played :initiative/played
           tokens :scene/initiative
           game-type-entity :scene/game-type}
          :camera/scene}
         :user/camera} result
        enabled-elements (:game-type/enabled-elements game-type-entity #{})
        on-quit (uix/use-callback #(dispatch :initiative/leave) [dispatch])
        on-next (uix/use-callback #(dispatch :initiative/next) [dispatch])]
    ($ :<>
      ($ :button.button.button-neutral
        {:disabled (empty? tokens) :on-click on-quit} "Leave")
      ;; Any enabled element that declares :initiative-actions gets its
      ;; own footer/bulk-level contribution rendered here (e.g. D&D's
      ;; "Roll Initiative for NPCs") -- this file never names a specific
      ;; game or mechanic, it only looks for the presence of that key.
      (for [[element-id element] (filter (comp :initiative-actions val)
                                          (select-keys game-type/elements enabled-elements))]
        ($ :<> {:key element-id}
          ((get-in element [:initiative-actions :render]) {:dispatch dispatch :tokens tokens})))
      (cond (not (seq tokens))
            ($ :button.button.button-neutral
              {:disabled true} "Next")
            (<= rounds 0)
            ($ :button.button.button-primary
              {:on-click on-next} "Start")
            (= (count played) (count tokens))
            ($ :button.button.button-primary
              {:on-click on-next} "New round")
            :else
            ($ :button.button.button-neutral
              {:on-click on-next} "Next")))))
