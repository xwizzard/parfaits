(ns ogres.app.component.panel-old-maid
  "The 'Old Maid' example game panel/tab -- a minimal UI over
   events.cljs's :old-maid/* methods, ogres.app.old-maid's pure pair-
   detection logic, and ogres.app.turn-order's shared turn-cycle logic.
   Visible to host and guest alike, same as Memory/Go Fish's panels.

   The first game ported onto the generic, nested mini-game session
   scaffolding (see events.cljs's 'Mini-game sessions' section and
   component/panel_minigame.cljs, whose session-list/new-session-form/
   controller-select this panel composes) -- several independent Old
   Maid tables, each seated by an arbitrary SUBSET of the roster (not
   necessarily every active player), can run at once on one scene.

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
            [ogres.app.component.card-hand :as card-hand]
            [ogres.app.component.panel-minigame :as minigame]
            [ogres.app.hooks :as hooks]
            [ogres.app.turn-order :as turn-order]
            [uix.core :refer [defui $]]))

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
           [:minigame/neutral-authority? :default false]
           {:minigame/deck
            [{:deck/cards
              [:db/id
               [:card/rank :default nil]
               [:card/label :default nil]
               [:card/icon :default nil]
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

(defn ^:private old-maid-sessions
  [scene]
  (filter (comp #{:old-maid} :minigame/kind) (:scene/minigames scene)))

(defn ^:private seat-controller-uuid
  "Mirrors events.cljs's minigame-controller-uuid -- a seat's own
   per-session override if assigned, else its roster player's
   ordinary :player/controller, else nil."
  [seat]
  (or (get-in seat [:seat/controller :user/uuid])
      (get-in seat [:seat/player :player/controller :user/uuid])))

(defn ^:private minigame-turn-player-id
  "Mirrors events.cljs's old-maid-turn-player -- the currently-active
   seat's player id at `minigame`, or nil, computed the same
   skip-eliminated way so the UI always agrees with what the next
   :old-maid/draw dispatch will actually validate against."
  [minigame]
  (let [seats (sort-by :seat/order (:minigame/seats minigame))
        players (mapv (comp :db/id :seat/player) seats)
        cards (:deck/cards (:minigame/deck minigame))
        players-by-id (into {} (map (juxt (comp :db/id :seat/player) :seat/player)) seats)
        hand-count-of (fn [id] (count (cards/cards-of-holder cards id)))
        active? (fn [id] (and (:player/active (players-by-id id)) (pos? (hand-count-of id))))
        idx (turn-order/valid-turn-index players active? (or (:minigame/turn-index minigame) 0))]
    (if idx (nth players idx))))

(defn ^:private selected-state
  "Derived state the hand/draw detail view needs for one selected
   session -- the session-scoped equivalent of what this panel used to
   compute directly off the scene before Old Maid became session-
   scoped (see events.cljs's own analogous helpers)."
  [minigame uuid host connected]
  (let [seats (sort-by :seat/order (:minigame/seats minigame))
        turn-players (mapv (comp :db/id :seat/player) seats)
        seat-by-player-id (into {} (map (juxt (comp :db/id :seat/player) identity)) seats)
        players-by-id (into {} (map (juxt (comp :db/id :seat/player) :seat/player)) seats)
        cards (:deck/cards (:minigame/deck minigame))
        neutral? (:minigame/neutral-authority? minigame)
        default-authority (and host (not neutral?))
        ;; Same split panel_go_fish.cljs already established: acting
        ;; for the current turn player (turn-continuity) must stay
        ;; host-fallback regardless of neutral-authority, distinct from
        ;; hand-visibility authority, which IS suppressed by it.
        turn-authority host
        hand-count-of (fn [id] (count (cards/cards-of-holder cards id)))
        active? (fn [id] (and (:player/active (players-by-id id)) (pos? (hand-count-of id))))
        current-index (turn-order/valid-turn-index turn-players active? (or (:minigame/turn-index minigame) 0))
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
        remaining (filter (fn [id] (pos? (hand-count-of id))) turn-players)
        current-seat (seat-by-player-id current-player-id)]
    {:seats seats
     :turn-players turn-players
     :current-index current-index
     :current-player-id current-player-id
     :target-player-id target-player-id
     :cards cards
     :draw-count (count (filter (comp #{:draw} :card/location) cards))
     :finished? (and (seq turn-players) (= (count remaining) 1))
     :loser-id (if (= (count remaining) 1) (first remaining))
     :my-turn? (and current-player-id
                    (card-hand/hand-authorized-with?
                     uuid turn-authority connected (if current-seat (seat-controller-uuid current-seat))))
     :default-authority default-authority}))

(defn ^:private old-maid-view
  "Shared derived view `panel`/`actions` both need, pulled once per
   render via the shared `query` above -- everything about the scene's
   Old Maid sessions and which one the local user is currently
   viewing."
  [result]
  (let [{uuid :user/uuid host :user/host viewing :user/minigame-viewing
         {scene :camera/scene} :user/camera} (:root/user result)
        sessions (old-maid-sessions scene)
        players (:root/players result)
        players-by-id (into {} (map (juxt :db/id identity)) players)
        connected (into #{} (map :user/uuid) (:session/conns (:root/session result)))
        viewing-id (:db/id viewing)
        selected (or (first (filter (comp #{viewing-id} :db/id) sessions)) (first sessions))]
    (merge
     {:uuid uuid :host host :players players :players-by-id players-by-id
      :connected connected :sessions sessions :selected selected}
     (if selected (selected-state selected uuid host connected)))))

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        publish (hooks/use-publish)
        result (hooks/use-query query [:db/ident :root])
        {:keys [uuid host players players-by-id connected sessions selected
                seats current-index target-player-id cards draw-count
                finished? loser-id my-turn? default-authority current-player-id]}
        (old-maid-view result)]
    ($ :.form-old-maid
      ($ :header ($ :h2 "Old Maid"))
      ($ minigame/session-list
        {:minigames sessions
         :selected-id (:db/id selected)
         :players-by-id players-by-id
         :turn-player-id-of minigame-turn-player-id
         :dispatch dispatch})
      (cond
        (not selected)
        ($ :.form-notice
          "Deal the entire deck (the traditional 52 minus 3 queens, one
           reskinned as the Old Maid) to whichever subset of the roster
           you pick below -- any matching pairs already in hand are
           discarded before play begins. On your turn, pick a card,
           unseen, from one of whoever's next in turn order's face-down
           cards -- any pair it completes discards automatically.
           Whoever's left holding only the Old Maid loses. Several
           tables can run at once, each with its own participants.")

        finished?
        ($ :.form-notice (str (:player/name (players-by-id loser-id)) " is the Old Maid!"))

        :else
        ($ :<>
          ($ :.old-maid-draw-count "Draw pile: " draw-count)
          ($ :ul.card-hands
            (for [[i seat] (map-indexed vector seats)
                  :let [entity (:seat/player seat)
                        controller-uuid (seat-controller-uuid seat)
                        authorized? (card-hand/hand-authorized-with? uuid default-authority connected controller-uuid)
                        pickable? (and my-turn? (= (:db/id entity) target-player-id))]]
              ($ card-hand/hand-view
                {:key (:db/id entity) :entity entity :cards cards :authorized? authorized?
                 :current? (= i current-index)
                 :show-backs? true
                 :back-card-click
                 (if pickable?
                   (fn [card] (dispatch :old-maid/draw (:db/id selected) current-player-id (:db/id card))))
                 :back-card-selectable? (if pickable? (constantly true))})))
          (if selected
            ($ :.old-maid-seats
              (for [seat seats]
                ($ :.old-maid-seat-controller
                  {:key (:db/id seat)}
                  ($ :span.old-maid-seat-controller-name (:player/name (:seat/player seat)))
                  ($ minigame/controller-select
                    {:seat seat
                     :conns (:session/conns (:root/session result))
                     :minigame-id (:db/id selected)
                     :dispatch dispatch})))))))
      ($ minigame/new-session-form
        {:players players
         :submit-label "Start table"
         :on-submit
         (fn [ids]
           (if host
             (dispatch :old-maid/start ids)
             (publish :minigame/create-request :old-maid ids)))}))))

(defui ^:memo actions []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        {:keys [selected finished?]} (old-maid-view result)]
    (if selected
      (if finished?
        ($ :button.button.button-neutral
          {:type "button" :on-click #(dispatch :minigame/remove (:db/id selected))}
          "New Game")
        ($ :button.button.button-danger
          {:type "button" :on-click #(dispatch :minigame/remove (:db/id selected))}
          ($ icon {:name "trash3-fill" :size 16})
          "End Table")))))
