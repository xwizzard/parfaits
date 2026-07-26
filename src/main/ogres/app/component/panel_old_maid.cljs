(ns ogres.app.component.panel-old-maid
  "The 'Old Maid' example game panel/tab -- a minimal UI over
   events.cljs's :old-maid/* methods, ogres.app.old-maid's pure pair-
   detection logic, and ogres.app.turn-order's shared turn-cycle logic.
   Visible to host and guest alike, same as Memory/Go Fish's panels.

   Simpler than panel_go_fish.cljs in one way (no rank picker -- a
   drawn card's identity is never chosen by rank) but with its own
   genuinely different interaction: you always draw from whoever's
   next in the turn cycle, but WHICH of their cards is up to you --
   their hand renders as face-down, individually-clickable
   placeholders (component/card_hand.cljs's show-backs?/back-card-
   click) instead of a single blind 'Draw' button, so picking a card
   is a real choice even though its rank stays hidden until it lands
   in your hand. No per-rank action button at all (pairs discard
   automatically, both at deal time and after every draw), and no
   :score. 'Finished' is exactly one player left holding cards --
   shown as the loser, not folded into any winner/tie phrasing."
  (:require [ogres.app.cards :as cards]
            [ogres.app.component :refer [icon]]
            [ogres.app.component.card-hand :as card-hand :refer [hand-authorized?]]
            [ogres.app.hooks :as hooks]
            [ogres.app.turn-order :as turn-order]
            [uix.core :refer [defui $]]))

(def ^:private query
  [{:root/user
    [:user/uuid
     :user/host
     {:user/camera
      [{:camera/scene
        [[:scene/old-maid-players :default nil]
         [:scene/old-maid-turn-index :default nil]
         [:scene/neutral-authority? :default false]
         {:scene/old-maid-deck
          [{:deck/cards
            [:db/id
             [:card/rank :default nil]
             [:card/label :default nil]
             [:card/icon :default nil]
             [:card/location :default nil]
             [:card/position :default 0]
             {:card/holder [:db/id]}]}]}]}]}]}
   {:root/players
    [:db/id :player/name :player/color :player/kind [:player/active :default true]
     {:player/controller [:user/uuid]}]}
   {:root/session [{:session/conns [:user/uuid]}]}])

(defn ^:private old-maid-state
  "Derived Old Maid state `panel`/`actions` both need, pulled once per
   render via the shared `query` above."
  [result]
  (let [{uuid :user/uuid host :user/host
         {scene :camera/scene} :user/camera} (:root/user result)
        players-by-id (into {} (map (juxt :db/id identity)) (:root/players result))
        connected (into #{} (map :user/uuid) (:session/conns (:root/session result)))
        {turn-players :scene/old-maid-players
         turn-index :scene/old-maid-turn-index
         neutral? :scene/neutral-authority?
         deck :scene/old-maid-deck} scene
        cards (:deck/cards deck)
        default-authority (and host (not neutral?))
        ;; Same split panel_go_fish.cljs already established: acting
        ;; for the current turn player (turn-continuity) must stay
        ;; host-fallback regardless of neutral-authority, distinct from
        ;; hand-visibility authority, which IS suppressed by it.
        turn-authority host
        hand-count-of (fn [id] (count (cards/cards-of-holder cards id)))
        active? (fn [id] (and (:player/active (players-by-id id)) (pos? (hand-count-of id))))
        current-index (turn-order/valid-turn-index turn-players active? (or turn-index 0))
        current-player-id (if current-index (nth turn-players current-index))
        ;; Who the current player would draw from -- resolved the same
        ;; way :old-maid/draw itself resolves it (from the CURRENT
        ;; player's own real index, skip-inactive), so this always
        ;; matches which hand the event will actually validate `card-
        ;; id` against, the same reactive-not-stale pattern panel_go_
        ;; fish.cljs's own next-player-id already established.
        next-index (if current-index
                     (turn-order/valid-turn-index turn-players active? (mod (inc current-index) (count turn-players))))
        target-player-id (if next-index (nth turn-players next-index))
        remaining (filter (fn [id] (pos? (hand-count-of id))) turn-players)]
    {:uuid uuid
     :host host
     :connected connected
     :default-authority default-authority
     :players-by-id players-by-id
     :turn-players turn-players
     :current-index current-index
     :current-player-id current-player-id
     :target-player-id target-player-id
     :cards cards
     :draw-count (count (filter (comp #{:draw} :card/location) cards))
     :started? (seq turn-players)
     :finished? (and (seq turn-players) (= (count remaining) 1))
     :loser-id (if (= (count remaining) 1) (first remaining))
     :my-turn? (and current-player-id
                    (hand-authorized? uuid turn-authority connected (players-by-id current-player-id)))}))

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        {:keys [uuid connected default-authority players-by-id turn-players
                current-index current-player-id target-player-id cards draw-count
                started? finished? loser-id my-turn?]}
        (old-maid-state result)]
    ($ :.form-old-maid
      ($ :header ($ :h2 "Old Maid"))
      (cond
        (not started?)
        ($ :.form-notice
          "Deal the entire deck (the traditional 52 minus 3 queens, one
           reskinned as the Old Maid) to every active roster player --
           any matching pairs already in hand are discarded before play
           begins. On your turn, pick a card, unseen, from one of
           whoever's next in turn order's face-down cards -- any pair
           it completes discards automatically. Whoever's left holding
           only the Old Maid loses -- add or bench participants from
           the Players tab before starting.")

        finished?
        ($ :.form-notice (str (:player/name (players-by-id loser-id)) " is the Old Maid!"))

        :else
        ($ :<>
          ($ :.old-maid-draw-count "Draw pile: " draw-count)
          ($ :ul.card-hands
            (for [[i id] (map-indexed vector turn-players)
                  :let [entity (players-by-id id)
                        authorized? (hand-authorized? uuid default-authority connected entity)
                        pickable? (and my-turn? (= id target-player-id))]]
              ($ card-hand/hand-view
                {:key id :entity entity :cards cards :authorized? authorized?
                 :current? (= i current-index)
                 :show-backs? true
                 :back-card-click (if pickable?
                                     (fn [card] (dispatch :old-maid/draw current-player-id (:db/id card))))
                 :back-card-selectable? (if pickable? (constantly true))}))))))))

(defui ^:memo actions []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        {:keys [host started? finished?]} (old-maid-state result)]
    ($ :<>
      (cond
        (not started?)
        (if host
          ($ :button.button.button-neutral
            {:type "button" :on-click #(dispatch :old-maid/start)}
            ($ icon {:name "skull" :size 16})
            "Start Old Maid"))

        finished?
        (if host
          ($ :button.button.button-neutral
            {:type "button" :on-click #(dispatch :old-maid/end)}
            "New Game"))

        :else
        (if host
          ($ :button.button.button-danger
            {:type "button" :on-click #(dispatch :old-maid/end)}
            ($ icon {:name "trash3-fill" :size 16})
            "End Game"))))))
