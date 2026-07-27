(ns ogres.app.component.panel-war
  "The 'War' example game panel/tab -- a minimal UI over events.cljs's
   :war/* methods and ogres.app.war's pure tie-detection logic.
   Visible to host and guest alike, same as every other example game's
   panel.

   The sixth and last game ported onto the generic, nested mini-game
   session scaffolding (see events.cljs's 'Mini-game sessions' section
   and component/panel_minigame.cljs, whose session-list/new-session-
   form this panel composes, same as the other ported games' panels)
   -- several independent War tables, each seated by an arbitrary
   subset of the roster, can run at once on one scene.

   Deliberately does NOT reuse component/card_hand.cljs's hand-view --
   War has no per-card interaction, no rank-grouping, and no privacy
   concept worth the name (nobody ever chooses anything, so there's
   nothing hidden that would change any decision); each player is just
   a name, a color, and a total card count. The one thing genuinely
   worth showing card-by-card is the shared pot (component/card_pile.
   cljs's pile-card, reused exactly as Rummy reuses it for its table
   groups), since that's what a 'War!' tie actually looks like on
   screen. A single host-only 'Play Round' button drives a selected
   table -- there's no per-seat action to gate, since nobody chooses a
   card."
  (:require [clojure.string :refer [join]]
            [ogres.app.component :refer [icon]]
            [ogres.app.component.card-pile :as card-pile]
            [ogres.app.component.panel-minigame :as minigame]
            [ogres.app.hooks :as hooks]
            [uix.core :refer [defui $]]))

(def ^:private query
  [{:root/user
    [:user/host
     {:user/minigame-viewing [:db/id]}
     {:user/camera
      [{:camera/scene
        [{:scene/minigames
          [:db/id
           :minigame/kind
           :minigame/label
           [:minigame/contenders :default nil]
           [:minigame/last-round :default nil]
           {:minigame/deck
            [{:deck/cards
              [:db/id
               [:card/rank :default nil]
               [:card/suit :default nil]
               [:card/label :default nil]
               [:card/icon :default nil]
               [:card/location :default nil]
               [:card/position :default 0]
               {:card/holder [:db/id]}]}]}
           {:minigame/seats
            [:db/id [:seat/order :default 0] {:seat/player [:db/id]}]}]}]}]}]}
   {:root/players
    [:db/id :player/name :player/color [:player/active :default true]]}])

(defn ^:private war-sessions [scene]
  (filter (comp #{:war} :minigame/kind) (:scene/minigames scene)))

(defn ^:private war-players [minigame]
  (mapv (comp :db/id :seat/player) (sort-by :seat/order (:minigame/seats minigame))))

(defn ^:private selected-state
  "Derived state the players/pot detail view needs for one selected
   session."
  [minigame players-by-id]
  (let [players (war-players minigame)
        contenders (:minigame/contenders minigame)
        last-round (:minigame/last-round minigame)
        cards (:deck/cards (:minigame/deck minigame))
        total-cards-of (fn [id]
                          (count (filter (fn [c] (and (contains? #{:draw :won} (:card/location c))
                                                       (= (:db/id (:card/holder c)) id)))
                                         cards)))
        active-ids (filter (fn [id] (and (:player/active (players-by-id id)) (pos? (total-cards-of id)))) players)
        pot-cards (filter (comp #{:war} :card/location) cards)]
    {:players players
     :contenders contenders
     :last-round last-round
     :total-cards-of total-cards-of
     :pot-cards pot-cards
     :finished? (and (seq players) (= (count active-ids) 1))
     :winner-id (if (= (count active-ids) 1) (first active-ids))}))

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        publish (hooks/use-publish)
        result (hooks/use-query query [:db/ident :root])
        {host :user/host viewing :user/minigame-viewing
         {scene :camera/scene} :user/camera} (:root/user result)
        sessions (war-sessions scene)
        roster (:root/players result)
        players-by-id (into {} (map (juxt :db/id identity)) roster)
        selected (or (first (filter (comp #{(:db/id viewing)} :db/id) sessions)) (first sessions))
        {:keys [players contenders last-round total-cards-of pot-cards finished? winner-id]}
        (if selected (selected-state selected players-by-id) {})]
    ($ :.form-war
      ($ :header ($ :h2 "War"))
      ($ minigame/session-list
        {:minigames sessions
         :selected-id (:db/id selected)
         :players-by-id players-by-id
         :turn-player-id-of (constantly nil)
         :dispatch dispatch})
      (cond
        (not selected)
        ($ :.form-notice
          "Deal the entire deck evenly to whichever subset of the
           roster you pick below -- each gets a personal draw pile,
           private and blind. Every round, every remaining player
           draws blindly from their own pile and reveals
           simultaneously -- the highest card wins the round and takes
           every card played. A tie forces a 'war': everyone still
           tied draws one more card and compares again, repeating
           until it breaks, with the whole pot going to whoever
           finally wins it. Once a player's draw pile runs out, their
           winnings shuffle into a fresh one -- running out of cards
           altogether eliminates them. Play continues until one player
           holds the entire deck. Several tables can run at once, each
           with its own participants.")

        finished?
        ($ :.form-notice (str (:player/name (players-by-id winner-id)) " wins with the entire deck!"))

        :else
        ($ :<>
          ($ :ul.war-players
            (for [id players
                  :let [entity (players-by-id id)
                        card-count (total-cards-of id)]]
              ($ :li.war-player {:key id :data-active (pos? card-count)}
                ($ :span.war-player-color {:data-color (:player/color entity)})
                ($ :span.war-player-name (:player/name entity))
                ($ :span.war-player-count card-count " cards"))))

          (cond
            (seq contenders)
            ($ :.war-banner
              "War! " (join " vs " (map (comp :player/name players-by-id) contenders))
              " -- pot: " (count pot-cards) " cards")

            last-round
            ($ :.war-banner
              (:player/name (players-by-id (:winner-id last-round)))
              " won the last round (+" (:cards-won last-round) " cards)"))

          (if (seq pot-cards)
            ($ :ul.war-pot
              (for [card (sort-by :card/position pot-cards)]
                ($ :li.war-pot-card {:key (:db/id card)}
                  ($ card-pile/pile-card {:card card})
                  ($ :span.war-pot-card-owner
                    (:player/name (players-by-id (:db/id (:card/holder card)))))))))))
      ($ minigame/new-session-form
        {:players roster
         :submit-label "Start table"
         :on-submit
         (fn [ids]
           (if host
             (dispatch :war/start ids)
             (publish :minigame/create-request :war ids)))}))))

(defui ^:memo actions []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        {host :user/host viewing :user/minigame-viewing
         {scene :camera/scene} :user/camera} (:root/user result)
        sessions (war-sessions scene)
        players-by-id (into {} (map (juxt :db/id identity)) (:root/players result))
        selected (or (first (filter (comp #{(:db/id viewing)} :db/id) sessions)) (first sessions))
        {:keys [finished?]} (if selected (selected-state selected players-by-id) {})]
    (if selected
      (if finished?
        ($ :button.button.button-neutral
          {:type "button" :on-click #(dispatch :minigame/remove (:db/id selected))}
          "New Game")
        (if host
          ($ :<>
            ($ :button.button.button-neutral
              {:type "button" :on-click #(dispatch :war/play-round (:db/id selected))}
              "Play Round")
            ($ :button.button.button-danger
              {:type "button" :on-click #(dispatch :minigame/remove (:db/id selected))}
              ($ icon {:name "trash3-fill" :size 16})
              "End Table")))))))
