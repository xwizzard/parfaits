(ns ogres.app.component.panel-war
  "The 'War' example game panel/tab -- a minimal UI over events.cljs's
   :war/* methods and ogres.app.war's pure tie-detection logic.
   Visible to host and guest alike, same as every other example game's
   panel.

   Deliberately does NOT reuse component/card_hand.cljs's hand-view --
   War has no per-card interaction, no rank-grouping, and no privacy
   concept worth the name (nobody ever chooses anything, so there's
   nothing hidden that would change any decision); each player is just
   a name, a color, and a total card count. The one thing genuinely
   worth showing card-by-card is the shared pot (component/card_pile.
   cljs's pile-card, reused exactly as Rummy reuses it for its table
   groups), since that's what a 'War!' tie actually looks like on
   screen. A single host-only 'Play Round' button drives the whole
   game -- there's no per-seat action to gate, since nobody chooses a
   card."
  (:require [clojure.string :refer [join]]
            [ogres.app.component :refer [icon]]
            [ogres.app.component.card-pile :as card-pile]
            [ogres.app.hooks :as hooks]
            [uix.core :refer [defui $]]))

(def ^:private query
  [{:root/user
    [:user/host
     {:user/camera
      [{:camera/scene
        [[:scene/war-players :default nil]
         [:scene/war-contenders :default nil]
         [:scene/war-last-round :default nil]
         {:scene/war-deck
          [{:deck/cards
            [:db/id
             [:card/rank :default nil]
             [:card/suit :default nil]
             [:card/label :default nil]
             [:card/icon :default nil]
             [:card/location :default nil]
             [:card/position :default 0]
             {:card/holder [:db/id]}]}]}]}]}]}
   {:root/players
    [:db/id :player/name :player/color [:player/active :default true]]}])

(defn ^:private war-state
  "Derived War state `panel`/`actions` both need, pulled once per
   render via the shared `query` above."
  [result]
  (let [{host :user/host {scene :camera/scene} :user/camera} (:root/user result)
        players-by-id (into {} (map (juxt :db/id identity)) (:root/players result))
        {players :scene/war-players
         contenders :scene/war-contenders
         last-round :scene/war-last-round
         deck :scene/war-deck} scene
        cards (:deck/cards deck)
        total-cards-of (fn [id]
                          (count (filter (fn [c] (and (contains? #{:draw :won} (:card/location c))
                                                       (= (:db/id (:card/holder c)) id)))
                                         cards)))
        active-ids (filter (fn [id] (and (:player/active (players-by-id id)) (pos? (total-cards-of id)))) players)
        pot-cards (filter (comp #{:war} :card/location) cards)]
    {:host host
     :players-by-id players-by-id
     :players players
     :contenders contenders
     :last-round last-round
     :total-cards-of total-cards-of
     :pot-cards pot-cards
     :started? (seq players)
     :finished? (and (seq players) (= (count active-ids) 1))
     :winner-id (if (= (count active-ids) 1) (first active-ids))}))

(defui ^:memo panel []
  (let [result (hooks/use-query query [:db/ident :root])
        {:keys [players-by-id players contenders last-round
                total-cards-of pot-cards started? finished? winner-id]}
        (war-state result)]
    ($ :.form-war
      ($ :header ($ :h2 "War"))
      (cond
        (not started?)
        ($ :.form-notice
          "Deal the entire deck evenly to every active roster player --
           each gets a personal draw pile, private and blind. Every
           round, every remaining player draws blindly from their own
           pile and reveals simultaneously -- the highest card wins
           the round and takes every card played. A tie forces a
           'war': everyone still tied draws one more card and compares
           again, repeating until it breaks, with the whole pot going
           to whoever finally wins it. Once a player's draw pile runs
           out, their winnings shuffle into a fresh one -- running out
           of cards altogether eliminates them. Play continues until
           one player holds the entire deck -- add or bench
           participants from the Players tab before starting.")

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
                    (:player/name (players-by-id (:db/id (:card/holder card))))))))))))))

(defui ^:memo actions []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        {:keys [host started? finished?]} (war-state result)]
    ($ :<>
      (cond
        (not started?)
        (if host
          ($ :button.button.button-neutral
            {:type "button" :on-click #(dispatch :war/start)}
            ($ icon {:name "fist" :size 16})
            "Start War"))

        finished?
        (if host
          ($ :button.button.button-neutral
            {:type "button" :on-click #(dispatch :war/end)}
            "New Game"))

        :else
        (if host
          ($ :<>
            ($ :button.button.button-neutral
              {:type "button" :on-click #(dispatch :war/play-round)}
              "Play Round")
            ($ :button.button.button-danger
              {:type "button" :on-click #(dispatch :war/end)}
              ($ icon {:name "trash3-fill" :size 16})
              "End Game")))))))
