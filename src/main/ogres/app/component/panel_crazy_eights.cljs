(ns ogres.app.component.panel-crazy-eights
  "The 'Crazy 8s' example game panel/tab -- a minimal UI over events.cljs's
   :crazy-eights/* methods, ogres.app.crazy-eights's pure per-card
   legality logic, and ogres.app.turn-order's shared turn-cycle logic.
   Visible to host and guest alike, same as Memory/Go Fish/Old Maid's
   panels.

   The one genuinely new piece none of the other three panels needed: a
   live, face-up shared pile (component/card_pile.cljs, also used by
   panel_decks.cljs) whose top card everyone's next play is checked
   against, and per-CARD (not per-rank-group) interactivity in the
   viewer's own hand (component/card_hand.cljs's on-card-click/card-
   playable? props) -- clicking a legal card plays it immediately,
   except an 8, which first opens a small suit picker since playing a
   wild also means declaring what the next player must match. A Draw
   button appears only once the viewer's hand holds no legal card at
   all -- drawing is never an optional alternative to a real play."
  (:require [ogres.app.cards :as cards]
            [ogres.app.component :refer [icon]]
            [ogres.app.component.card-hand :as card-hand :refer [hand-authorized?]]
            [ogres.app.component.card-pile :as card-pile]
            [ogres.app.crazy-eights :as crazy-eights]
            [ogres.app.hooks :as hooks]
            [ogres.app.turn-order :as turn-order]
            [uix.core :as uix :refer [defui $]]))

(def ^:private query
  [{:root/user
    [:user/uuid
     :user/host
     {:user/camera
      [{:camera/scene
        [[:scene/crazy-eights-players :default nil]
         [:scene/crazy-eights-turn-index :default nil]
         [:scene/crazy-eights-suit :default nil]
         [:scene/crazy-eights-winner :default nil]
         [:scene/neutral-authority? :default false]
         {:scene/crazy-eights-deck
          [{:deck/cards
            [:db/id
             [:card/rank :default nil]
             [:card/suit :default nil]
             [:card/label :default nil]
             [:card/icon :default nil]
             [:card/icon-color :default nil]
             [:card/location :default nil]
             [:card/position :default 0]
             {:card/holder [:db/id]}]}]}]}]}]}
   {:root/players
    [:db/id :player/name :player/color :player/kind [:player/active :default true]
     {:player/controller [:user/uuid]}]}
   {:root/session [{:session/conns [:user/uuid]}]}])

(defn ^:private icon-for-suit
  "The :card/icon any card of `suit` in `cards` uses -- reused for the
   suit-picker buttons rather than hand-rolling a second suit->icon
   map alongside game_type/core_decks.cljs's own (private) one."
  [cards suit]
  (some #(if (= (:card/suit %) suit) (:card/icon %)) cards))

(defn ^:private crazy-eights-state
  "Derived Crazy 8s state `panel`/`actions` both need, pulled once per
   render via the shared `query` above."
  [result]
  (let [{uuid :user/uuid host :user/host
         {scene :camera/scene} :user/camera} (:root/user result)
        players-by-id (into {} (map (juxt :db/id identity)) (:root/players result))
        connected (into #{} (map :user/uuid) (:session/conns (:root/session result)))
        {turn-players :scene/crazy-eights-players
         turn-index :scene/crazy-eights-turn-index
         declared-suit :scene/crazy-eights-suit
         winner-id :scene/crazy-eights-winner
         neutral? :scene/neutral-authority?
         deck :scene/crazy-eights-deck} scene
        cards (:deck/cards deck)
        default-authority (and host (not neutral?))
        ;; Same split panel_go_fish.cljs/panel_old_maid.cljs already
        ;; established: acting for the current turn player (turn-
        ;; continuity) must stay host-fallback regardless of neutral-
        ;; authority, distinct from hand-visibility authority, which
        ;; IS suppressed by it.
        turn-authority host
        active? (fn [id] (:player/active (players-by-id id)))
        current-index (turn-order/valid-turn-index turn-players active? (or turn-index 0))
        current-player-id (if current-index (nth turn-players current-index))
        top (card-pile/top-of cards :discard)
        current-hand (if current-player-id (cards/cards-of-holder cards current-player-id))
        legal-ids (into #{} (map :db/id) (crazy-eights/playable-cards current-hand top declared-suit))]
    {:uuid uuid
     :host host
     :connected connected
     :default-authority default-authority
     :players-by-id players-by-id
     :turn-players turn-players
     :current-index current-index
     :current-player-id current-player-id
     :cards cards
     :top top
     :declared-suit declared-suit
     :legal-ids legal-ids
     :draw-count (count (filter (comp #{:draw} :card/location) cards))
     :started? (seq turn-players)
     :finished? (some? winner-id)
     :winner-id winner-id
     :my-turn? (and current-player-id
                    (hand-authorized? uuid turn-authority connected (players-by-id current-player-id)))
     :can-draw? (and current-player-id (empty? legal-ids))}))

(defui ^:private suit-picker [{:keys [cards on-pick on-cancel]}]
  ($ :.crazy-eights-suit-picker
    ($ :span "Choose a suit for your Crazy Eight:")
    ($ :.crazy-eights-suit-picker-buttons
      (for [suit [:clubs :diamonds :hearts :spades]]
        ($ :button.button.button-neutral
          {:key suit :type "button" :on-click #(on-pick suit)}
          ($ icon {:name (icon-for-suit cards suit) :size 16})))
      ($ :button.button.button-neutral {:type "button" :on-click on-cancel} "Cancel"))))

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        {:keys [uuid connected default-authority players-by-id turn-players
                current-index current-player-id cards top declared-suit legal-ids
                draw-count started? finished? winner-id my-turn? can-draw?]}
        (crazy-eights-state result)
        [pending-eight set-pending-eight] (uix/use-state nil)]
    ($ :.form-crazy-eights
      ($ :header ($ :h2 "Crazy 8s"))
      (cond
        (not started?)
        ($ :.form-notice
          "Deal 6 cards to each active roster player from a standard
           52-card deck (its four 8s are wild, suit-less cards) and
           flip the next card face up as the starting discard. On your
           turn, play a card matching the discard pile's top card by
           rank or suit, or play any 8 and name the suit the next
           player must match. No legal card in hand? Draw one at a
           time until you have one. First to discard every card wins
           -- add or bench participants from the Players tab before
           starting.")

        finished?
        ($ :.form-notice (str (:player/name (players-by-id winner-id)) " wins!"))

        :else
        ($ :<>
          ($ :.crazy-eights-piles
            ($ :.crazy-eights-pile
              ($ card-pile/pile-card {:back? true :count draw-count})
              ($ :span "Draw"))
            ($ :.crazy-eights-pile
              ($ card-pile/pile-card {:card top})
              ($ :span "Discard"))
            (if (= (:card/rank top) :eight)
              ($ :.crazy-eights-declared-suit
                "Suit in play: "
                ($ icon {:name (icon-for-suit cards declared-suit) :size 16}))))
          ($ :ul.card-hands
            (for [[i id] (map-indexed vector turn-players)
                  :let [entity (players-by-id id)
                        acting? (and my-turn? (= id current-player-id))
                        ;; Unlike Go Fish's rank+target picker or Old
                        ;; Maid's single blind-draw button, playing a
                        ;; card here means clicking a SPECIFIC visible
                        ;; card -- so a viewer standing in for this
                        ;; seat's turn via host-fallback (turn-
                        ;; authority, unaffected by neutral-authority)
                        ;; must also be able to see the hand they're
                        ;; playing from, not just act on it blind.
                        authorized? (or (hand-authorized? uuid default-authority connected entity) acting?)]]
              ($ card-hand/hand-view
                {:key id :entity entity :cards cards :authorized? authorized?
                 :current? (= i current-index)
                 :card-playable? (if acting? (fn [card] (contains? legal-ids (:db/id card))))
                 :on-card-click
                 (if acting?
                   (fn [card]
                     (if (= (:card/rank card) :eight)
                       (set-pending-eight (:db/id card))
                       (dispatch :crazy-eights/play current-player-id (:db/id card) nil))))})))
          (if my-turn?
            (cond
              pending-eight
              ($ suit-picker
                {:cards cards
                 :on-pick (fn [suit]
                            (dispatch :crazy-eights/play current-player-id pending-eight suit)
                            (set-pending-eight nil))
                 :on-cancel #(set-pending-eight nil)})

              can-draw?
              ($ :button.button.button-neutral
                {:type "button" :on-click #(dispatch :crazy-eights/draw current-player-id)}
                "Draw"))))))))

(defui ^:memo actions []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        {:keys [host started? finished?]} (crazy-eights-state result)]
    ($ :<>
      (cond
        (not started?)
        (if host
          ($ :button.button.button-neutral
            {:type "button" :on-click #(dispatch :crazy-eights/start)}
            ($ icon {:name "magic" :size 16})
            "Start Crazy 8s"))

        finished?
        (if host
          ($ :button.button.button-neutral
            {:type "button" :on-click #(dispatch :crazy-eights/end)}
            "New Game"))

        :else
        (if host
          ($ :button.button.button-danger
            {:type "button" :on-click #(dispatch :crazy-eights/end)}
            ($ icon {:name "trash3-fill" :size 16})
            "End Game"))))))
