(ns ogres.app.component.panel-crazy-eights
  "The 'Crazy 8s' example game panel/tab -- a minimal UI over events.cljs's
   :crazy-eights/* methods, ogres.app.crazy-eights's pure per-card
   legality logic, and ogres.app.turn-order's shared turn-cycle logic.
   Visible to host and guest alike, same as Memory/Go Fish/Old Maid's
   panels.

   The fourth game ported onto the generic, nested mini-game session
   scaffolding (see events.cljs's 'Mini-game sessions' section and
   component/panel_minigame.cljs, whose session-list/new-session-form
   this panel composes, same as the other ported games' panels) --
   several independent Crazy 8s tables, each seated by an arbitrary
   subset of the roster, can run at once on one scene.

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
            [ogres.app.component.card-hand :as card-hand]
            [ogres.app.component.card-pile :as card-pile]
            [ogres.app.component.panel-minigame :as minigame]
            [ogres.app.crazy-eights :as crazy-eights]
            [ogres.app.hooks :as hooks]
            [ogres.app.turn-order :as turn-order]
            [uix.core :as uix :refer [defui $]]))

(def ^:private query
  [{:root/user
    [:user/uuid
     :user/host
     {:user/minigame-viewing [:db/id]}
     {:user/camera
      [{:camera/scene
        [{:scene/minigames
          [:db/id
           :minigame/kind
           :minigame/label
           [:minigame/turn-index :default 0]
           [:minigame/suit :default nil]
           [:minigame/winner :default nil]
           [:minigame/neutral-authority? :default false]
           {:minigame/deck
            [{:deck/cards
              [:db/id
               [:card/rank :default nil]
               [:card/suit :default nil]
               [:card/label :default nil]
               [:card/icon :default nil]
               [:card/icon-color :default nil]
               [:card/location :default nil]
               [:card/position :default 0]
               {:card/holder [:db/id]}]}]}
           {:minigame/seats
            [:db/id
             [:seat/order :default 0]
             {:seat/player
              [:db/id :player/name :player/color :player/kind
               [:player/active :default true]
               {:player/controller [:user/uuid]}]}
             {:seat/controller [:user/uuid]}]}]}]}]}]}
   {:root/players
    [:db/id :player/name :player/kind [:player/active :default true]]}
   {:root/session [{:session/conns [:db/id :user/uuid :user/color :user/label]}]}])

(defn ^:private crazy-eights-sessions [scene]
  (filter (comp #{:crazy-eights} :minigame/kind) (:scene/minigames scene)))

(defn ^:private seat-controller-uuid
  "Mirrors events.cljs's minigame-controller-uuid."
  [seat]
  (or (get-in seat [:seat/controller :user/uuid])
      (get-in seat [:seat/player :player/controller :user/uuid])))

(defn ^:private icon-for-suit
  "The :card/icon any card of `suit` in `cards` uses -- reused for the
   suit-picker buttons rather than hand-rolling a second suit->icon
   map alongside game_type/core_decks.cljs's own (private) one."
  [cards suit]
  (some #(if (= (:card/suit %) suit) (:card/icon %)) cards))

(defn ^:private crazy-eights-turn-player-id
  "Mirrors events.cljs's crazy-eights-turn-player -- the currently-
   active seat's player id at `minigame`, or nil."
  [minigame]
  (let [seats (sort-by :seat/order (:minigame/seats minigame))
        players (mapv (comp :db/id :seat/player) seats)
        players-by-id (into {} (map (juxt (comp :db/id :seat/player) :seat/player)) seats)
        active? (fn [id] (:player/active (players-by-id id)))
        idx (turn-order/valid-turn-index players active? (or (:minigame/turn-index minigame) 0))]
    (if idx (nth players idx))))

(defn ^:private selected-state
  "Derived state the discard/hand/draw detail view needs for one
   selected session."
  [minigame uuid host connected]
  (let [seats (sort-by :seat/order (:minigame/seats minigame))
        turn-players (mapv (comp :db/id :seat/player) seats)
        players-by-id (into {} (map (juxt (comp :db/id :seat/player) :seat/player)) seats)
        cards (:deck/cards (:minigame/deck minigame))
        winner-id (:minigame/winner minigame)
        neutral? (:minigame/neutral-authority? minigame)
        default-authority (and host (not neutral?))
        ;; Same split panel_go_fish.cljs/panel_old_maid.cljs already
        ;; established: acting for the current turn player (turn-
        ;; continuity) must stay host-fallback regardless of neutral-
        ;; authority, distinct from hand-visibility authority, which
        ;; IS suppressed by it.
        turn-authority host
        active? (fn [id] (:player/active (players-by-id id)))
        current-index (turn-order/valid-turn-index turn-players active? (or (:minigame/turn-index minigame) 0))
        current-player-id (if current-index (nth turn-players current-index))
        declared-suit (:minigame/suit minigame)
        top (card-pile/top-of cards :discard)
        current-hand (if current-player-id (cards/cards-of-holder cards current-player-id))
        legal-ids (into #{} (map :db/id) (crazy-eights/playable-cards current-hand top declared-suit))]
    {:seats seats
     :turn-players turn-players
     :current-index current-index
     :current-player-id current-player-id
     :cards cards
     :top top
     :declared-suit declared-suit
     :legal-ids legal-ids
     :draw-count (count (filter (comp #{:draw} :card/location) cards))
     :finished? (some? winner-id)
     :winner-id winner-id
     :default-authority default-authority
     :my-turn? (and current-player-id
                    (card-hand/hand-authorized-with?
                     uuid turn-authority connected
                     (seat-controller-uuid (some #(if (= (:db/id (:seat/player %)) current-player-id) %) seats))))
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
        publish (hooks/use-publish)
        result (hooks/use-query query [:db/ident :root])
        {uuid :user/uuid host :user/host viewing :user/minigame-viewing
         {scene :camera/scene} :user/camera} (:root/user result)
        sessions (crazy-eights-sessions scene)
        players (:root/players result)
        players-by-id (into {} (map (juxt :db/id identity)) players)
        connected (into #{} (map :user/uuid) (:session/conns (:root/session result)))
        selected (or (first (filter (comp #{(:db/id viewing)} :db/id) sessions)) (first sessions))
        {:keys [seats current-index current-player-id cards top declared-suit legal-ids
                draw-count finished? winner-id my-turn? can-draw? default-authority]}
        (if selected (selected-state selected uuid host connected) {})
        [pending-eight set-pending-eight] (uix/use-state nil)]
    ($ :.form-crazy-eights
      ($ :header ($ :h2 "Crazy 8s"))
      ($ minigame/session-list
        {:minigames sessions
         :selected-id (:db/id selected)
         :players-by-id players-by-id
         :turn-player-id-of crazy-eights-turn-player-id
         :dispatch dispatch})
      (cond
        (not selected)
        ($ :.form-notice
          "Deal 6 cards to each participant you pick below, from a
           standard 52-card deck (its four 8s are wild, suit-less
           cards), and flip the next card face up as the starting
           discard. On your turn, play a card matching the discard
           pile's top card by rank or suit, or play any 8 and name the
           suit the next player must match. No legal card in hand? Draw
           one at a time until you have one. First to discard every
           card wins. Several tables can run at once, each with its own
           participants.")

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
            (for [[i seat] (map-indexed vector seats)
                  :let [entity (:seat/player seat)
                        id (:db/id entity)
                        acting? (and my-turn? (= id current-player-id))
                        ;; Unlike Go Fish's rank+target picker or Old
                        ;; Maid's single blind-draw button, playing a
                        ;; card here means clicking a SPECIFIC visible
                        ;; card -- so a viewer standing in for this
                        ;; seat's turn via host-fallback (turn-
                        ;; authority, unaffected by neutral-authority)
                        ;; must also be able to see the hand they're
                        ;; playing from, not just act on it blind.
                        authorized? (or (card-hand/hand-authorized-with?
                                         uuid default-authority connected (seat-controller-uuid seat))
                                        acting?)]]
              ($ card-hand/hand-view
                {:key id :entity entity :cards cards :authorized? authorized?
                 :current? (= i current-index)
                 :card-playable? (if acting? (fn [card] (contains? legal-ids (:db/id card))))
                 :on-card-click
                 (if acting?
                   (fn [card]
                     (if (= (:card/rank card) :eight)
                       (set-pending-eight (:db/id card))
                       (dispatch :crazy-eights/play (:db/id selected) current-player-id (:db/id card) nil))))})))
          (if my-turn?
            (cond
              pending-eight
              ($ suit-picker
                {:cards cards
                 :on-pick (fn [suit]
                            (dispatch :crazy-eights/play (:db/id selected) current-player-id pending-eight suit)
                            (set-pending-eight nil))
                 :on-cancel #(set-pending-eight nil)})

              can-draw?
              ($ :button.button.button-neutral
                {:type "button" :on-click #(dispatch :crazy-eights/draw (:db/id selected) current-player-id)}
                "Draw")))))
      ($ minigame/new-session-form
        {:players players
         :submit-label "Start table"
         :on-submit
         (fn [ids]
           (if host
             (dispatch :crazy-eights/start ids)
             (publish :minigame/create-request :crazy-eights ids)))}))))

(defui ^:memo actions []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        {uuid :user/uuid host :user/host viewing :user/minigame-viewing
         {scene :camera/scene} :user/camera} (:root/user result)
        sessions (crazy-eights-sessions scene)
        connected (into #{} (map :user/uuid) (:session/conns (:root/session result)))
        selected (or (first (filter (comp #{(:db/id viewing)} :db/id) sessions)) (first sessions))
        {:keys [finished?]} (if selected (selected-state selected uuid host connected) {})]
    (if selected
      (if finished?
        ($ :button.button.button-neutral
          {:type "button" :on-click #(dispatch :minigame/remove (:db/id selected))}
          "New Game")
        ($ :button.button.button-danger
          {:type "button" :on-click #(dispatch :minigame/remove (:db/id selected))}
          ($ icon {:name "trash3-fill" :size 16})
          "End Table")))))
