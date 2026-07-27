(ns ogres.app.component.panel-rummy
  "The 'Rummy' example game panel/tab -- a minimal UI over events.cljs's
   :rummy/* methods, ogres.app.rummy's pure set/run detection logic,
   and ogres.app.turn-order's shared turn-cycle/winner logic. Visible
   to host and guest alike, same as the other four panels.

   The fifth game ported onto the generic, nested mini-game session
   scaffolding (see events.cljs's 'Mini-game sessions' section and
   component/panel_minigame.cljs, whose session-list/new-session-form
   this panel composes, same as the other ported games' panels) --
   several independent Rummy tables, each seated by an arbitrary
   subset of the roster, can run at once on one scene.

   The two genuinely new pieces none of the other four needed: a
   'Table' section showing every rank currently scored -- a SHARED
   pile any player may contribute to, not credited to one game module
   the way Go Fish's :scored cards are shown; and card_hand.cljs's
   `render-group-extra`/`on-card-click`+`card-playable?` extension
   points used TOGETHER for the first time (Go Fish only ever used the
   former, Crazy 8s only the latter) -- a per-rank 'Score' button on
   every authorized seat's hand (scoring is never turn-gated, exactly
   like :go-fish/score), alongside per-card click-to-discard on the
   viewer's own seat, only once it's their turn and they've already
   drawn. `:minigame/drawn?` gates a 'Draw from pile'/'Take discard'
   choice before that -- one draw, from either pile, per turn.
   Optional run-melds (:rummy/runs, off by default) get their own
   small per-seat list below the hands, same not-turn-gated
   authorization as sets."
  (:require [clojure.string :refer [join]]
            [ogres.app.cards :as cards]
            [ogres.app.component :refer [icon]]
            [ogres.app.component.card-hand :as card-hand]
            [ogres.app.component.card-pile :as card-pile :refer [rank-short]]
            [ogres.app.component.panel-minigame :as minigame]
            [ogres.app.hooks :as hooks]
            [ogres.app.rummy :as rummy]
            [ogres.app.turn-order :as turn-order]
            [uix.core :refer [defui $]]))

(def ^:private query
  [{:root/user
    [:user/uuid
     :user/host
     {:user/minigame-viewing [:db/id]}
     {:user/camera
      [{:camera/scene
        [{:scene/game-type [[:game-type/enabled-elements :default #{}]]}
         {:scene/minigames
          [:db/id
           :minigame/kind
           :minigame/label
           [:minigame/turn-index :default 0]
           [:minigame/drawn? :default false]
           [:minigame/neutral-authority? :default false]
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

(defn ^:private rummy-sessions [scene]
  (filter (comp #{:rummy} :minigame/kind) (:scene/minigames scene)))

(defn ^:private seat-controller-uuid
  "Mirrors events.cljs's minigame-controller-uuid."
  [seat]
  (or (get-in seat [:seat/controller :user/uuid])
      (get-in seat [:seat/player :player/controller :user/uuid])))

(defn ^:private rummy-turn-player-id
  "Mirrors events.cljs's rummy-turn-player -- the currently-active
   seat's player id at `minigame`, or nil."
  [minigame]
  (let [seats (sort-by :seat/order (:minigame/seats minigame))
        players (mapv (comp :db/id :seat/player) seats)
        players-by-id (into {} (map (juxt (comp :db/id :seat/player) :seat/player)) seats)
        active? (fn [id] (:player/active (players-by-id id)))
        idx (turn-order/valid-turn-index players active? (or (:minigame/turn-index minigame) 0))]
    (if idx (nth players idx))))

(defn ^:private selected-state
  "Derived state the table/hand/draw detail view needs for one selected
   session."
  [minigame enabled-elements uuid host connected]
  (let [seats (sort-by :seat/order (:minigame/seats minigame))
        turn-players (mapv (comp :db/id :seat/player) seats)
        players-by-id (into {} (map (juxt (comp :db/id :seat/player) :seat/player)) seats)
        drawn? (:minigame/drawn? minigame)
        neutral? (:minigame/neutral-authority? minigame)
        cards (:deck/cards (:minigame/deck minigame))
        default-authority (and host (not neutral?))
        ;; Same split every prior game's panel already established:
        ;; acting for the current turn player (turn-continuity) must
        ;; stay host-fallback regardless of neutral-authority, distinct
        ;; from hand-visibility authority, which IS suppressed by it.
        turn-authority host
        active? (fn [id] (:player/active (players-by-id id)))
        current-index (turn-order/valid-turn-index turn-players active? (or (:minigame/turn-index minigame) 0))
        current-player-id (if current-index (nth turn-players current-index))
        current-seat (some #(if (= (:db/id (:seat/player %)) current-player-id) %) seats)
        scored (filter (comp #{:scored} :card/location) cards)
        scores (frequencies (keep (comp :db/id :card/holder) scored))]
    {:seats seats
     :turn-players turn-players
     :current-index current-index
     :current-player-id current-player-id
     :cards cards
     :scored scored
     :scores scores
     :draw-count (count (filter (comp #{:draw} :card/location) cards))
     :discard-top (card-pile/top-of cards :discard)
     :runs-enabled? (contains? enabled-elements :rummy/runs)
     :drawn? drawn?
     :default-authority default-authority
     :finished? (and (seq turn-players)
                      (some (fn [id] (empty? (cards/cards-of-holder cards id))) turn-players))
     :my-turn? (and current-player-id
                    (card-hand/hand-authorized-with?
                     uuid turn-authority connected (if current-seat (seat-controller-uuid current-seat))))}))

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        publish (hooks/use-publish)
        result (hooks/use-query query [:db/ident :root])
        {uuid :user/uuid host :user/host viewing :user/minigame-viewing
         {scene :camera/scene} :user/camera} (:root/user result)
        sessions (rummy-sessions scene)
        players (:root/players result)
        players-by-id (into {} (map (juxt :db/id identity)) players)
        connected (into #{} (map :user/uuid) (:session/conns (:root/session result)))
        enabled-elements (:game-type/enabled-elements (:scene/game-type scene))
        selected (or (first (filter (comp #{(:db/id viewing)} :db/id) sessions)) (first sessions))
        {:keys [seats current-index current-player-id cards scored scores draw-count
                discard-top runs-enabled? drawn? finished? my-turn? default-authority]}
        (if selected (selected-state selected enabled-elements uuid host connected) {})
        ;; A seat's hand is visible to the viewer if the normal
        ;; (neutral-suppressed) authority applies, OR they have host-
        ;; fallback authority for this SPECIFIC seat (standing in for
        ;; an unassigned/disconnected controller) -- needed because
        ;; both of Rummy's actions (discard, score) mean clicking a
        ;; specific card, unlike Old Maid's/Go Fish's single-button/
        ;; dropdown actions, so a viewer allowed to act for a seat must
        ;; also be able to see what they're acting on (the same fix
        ;; Crazy 8s' panel already applies for its own turn-gated
        ;; play action -- here it also covers :rummy/score, which
        ;; isn't even turn-gated at all).
        seat-visible? (fn [seat] (or (card-hand/hand-authorized-with? uuid default-authority connected (seat-controller-uuid seat))
                                      (card-hand/hand-authorized-with? uuid host connected (seat-controller-uuid seat))))]
    ($ :.form-rummy
      ($ :header ($ :h2 "Rummy"))
      ($ minigame/session-list
        {:minigames sessions
         :selected-id (:db/id selected)
         :players-by-id players-by-id
         :turn-player-id-of rummy-turn-player-id
         :dispatch dispatch})
      (cond
        (not selected)
        ($ :.form-notice
          "Deal 6 cards to each participant you pick below, from a
           standard 52-card deck, and flip the next card face up as the
           starting discard. On your turn, take one card -- from the
           draw pile or the discard pile -- then end your turn by
           discarding a card. Any time, lay down 3 or more cards of the
           same rank as a scored set on the table, or add a 4th to an
           existing one even if someone else started it. Play ends the
           moment anyone empties their hand -- whoever holds the most
           scored cards at that point wins, not necessarily whoever ran
           out first. Several tables can run at once, each with its own
           participants.")

        finished?
        (let [winner-ids (turn-order/winners scores)]
          ($ :.form-notice
            (if (= (count winner-ids) 1)
              (str (:player/name (players-by-id (first winner-ids))) " wins with "
                   (get scores (first winner-ids) 0) " scored cards!")
              (str "Tied: " (join ", " (map (comp :player/name players-by-id) winner-ids))))))

        :else
        ($ :<>
          ($ :.rummy-piles
            ($ :.rummy-pile
              ($ card-pile/pile-card {:back? true :count draw-count})
              ($ :span "Draw"))
            ($ :.rummy-pile
              ($ card-pile/pile-card {:card discard-top})
              ($ :span "Discard")))

          (if (seq scored)
            ($ :.rummy-table
              ($ :h3 "Table")
              ($ :ul.rummy-table-groups
                (for [[rank group] (sort-by (comp rank-short key) (group-by :card/rank scored))]
                  ($ :li.rummy-table-group {:key rank}
                    (for [card group] ($ card-pile/pile-card {:key (:db/id card) :card card})))))))

          ($ :ul.card-hands
            (for [[i seat] (map-indexed vector seats)
                  :let [entity (:seat/player seat)
                        id (:db/id entity)
                        authorized? (seat-visible? seat)
                        acting? (and my-turn? (= id current-player-id) drawn?)]]
              ($ card-hand/hand-view
                {:key id :entity entity :cards cards :authorized? authorized?
                 :current? (= i current-index) :score (get scores id 0)
                 :card-playable? (if acting? (constantly true))
                 :on-card-click (if acting? (fn [card] (dispatch :rummy/discard (:db/id selected) id (:db/id card))))
                 :render-group-extra
                 (fn [rank group]
                   (let [already (count (cards/cards-of-rank scored rank))
                         n (rummy/scoreable-set (count group) already)]
                     (if (pos? n)
                       ($ :button.button.button-neutral.rummy-score-button
                         {:type "button" :on-click #(dispatch :rummy/score (:db/id selected) id rank)}
                         "Score"))))})))

          (if runs-enabled?
            (for [seat seats
                  :let [entity (:seat/player seat)
                        id (:db/id entity)
                        authorized? (seat-visible? seat)
                        available-runs (if authorized? (rummy/runs (cards/cards-of-holder cards id)))]
                  :when (seq available-runs)]
              ($ :.rummy-runs {:key id}
                ($ :span.rummy-runs-label (:player/name entity) "'s runs: ")
                (for [run available-runs]
                  ($ :button.button.button-neutral.rummy-run-button
                    {:key (join "-" (map :db/id run))
                     :type "button"
                     :on-click #(dispatch :rummy/score-run (:db/id selected) id (mapv :db/id run))}
                    (join " " (map (comp rank-short :card/rank) run)))))))

          (if (and my-turn? (not drawn?))
            ($ :.rummy-draw-controls
              ($ :button.button.button-neutral
                {:type "button" :on-click #(dispatch :rummy/draw-from-pile (:db/id selected) current-player-id)}
                "Draw from pile")
              ($ :button.button.button-neutral
                {:type "button" :disabled (nil? discard-top)
                 :on-click #(dispatch :rummy/draw-from-discard (:db/id selected) current-player-id)}
                "Take discard")))))
      ($ minigame/new-session-form
        {:players players
         :submit-label "Start table"
         :on-submit
         (fn [ids]
           (if host
             (dispatch :rummy/start ids)
             (publish :minigame/create-request :rummy ids)))}))))

(defui ^:memo actions []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        {uuid :user/uuid host :user/host viewing :user/minigame-viewing
         {scene :camera/scene} :user/camera} (:root/user result)
        sessions (rummy-sessions scene)
        connected (into #{} (map :user/uuid) (:session/conns (:root/session result)))
        enabled-elements (:game-type/enabled-elements (:scene/game-type scene))
        selected (or (first (filter (comp #{(:db/id viewing)} :db/id) sessions)) (first sessions))
        {:keys [finished?]} (if selected (selected-state selected enabled-elements uuid host connected) {})]
    (if selected
      (if finished?
        ($ :button.button.button-neutral
          {:type "button" :on-click #(dispatch :minigame/remove (:db/id selected))}
          "New Game")
        ($ :button.button.button-danger
          {:type "button" :on-click #(dispatch :minigame/remove (:db/id selected))}
          ($ icon {:name "trash3-fill" :size 16})
          "End Table")))))
