(ns ogres.app.component.panel-go-fish
  "The 'Go Fish' example game panel/tab -- a minimal UI over events.cljs's
   :go-fish/* methods, ogres.app.go-fish's pure rank-matching/scoring
   logic, and ogres.app.turn-order's shared turn-cycle/winner logic.
   Visible to host and guest alike, same as Memory's panel.

   The third game ported onto the generic, nested mini-game session
   scaffolding (see events.cljs's 'Mini-game sessions' section and
   component/panel_minigame.cljs, whose session-list/new-session-form
   this panel composes, same as panel_old_maid.cljs/panel_memory.cljs)
   -- several independent Go Fish tables, each seated by an arbitrary
   subset of the roster, can run at once on one scene.

   The one genuinely new piece Memory's panel never needed: hand
   privacy. Every seat's hand is checked independently via
   player/authority? (the same primitive scene_objects.cljs/
   panel_initiative.cljs already use for canvas-object/token
   visibility, just pointed at a roster player's own controller instead
   of an object's owner) -- authorized hands render real, rank-grouped
   cards with a Score button wherever ogres.app.go-fish/scoreable-count
   currently allows one; unauthorized hands render a face-down count
   only, matching Memory's own hidden-card placeholder convention.
   :minigame/neutral-authority? (auto-enabled by :go-fish/start, same
   as Memory) keeps the host from automatically seeing every hand just
   because they're host -- the same 'host is often also a competing
   player' reasoning applies here."
  (:require [clojure.string :refer [join]]
            [ogres.app.cards :as cards]
            [ogres.app.component :refer [icon]]
            [ogres.app.component.card-hand :as card-hand]
            [ogres.app.component.card-pile :refer [rank-short]]
            [ogres.app.component.panel-minigame :as minigame]
            [ogres.app.go-fish :as go-fish]
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
        [{:scene/game-type [[:game-type/enabled-elements :default #{}]]}
         {:scene/minigames
          [:db/id
           :minigame/kind
           :minigame/label
           [:minigame/turn-index :default 0]
           [:minigame/scores :default nil]
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

(defn ^:private go-fish-sessions [scene]
  (filter (comp #{:go-fish} :minigame/kind) (:scene/minigames scene)))

(defn ^:private seat-controller-uuid
  "Mirrors events.cljs's minigame-controller-uuid."
  [seat]
  (or (get-in seat [:seat/controller :user/uuid])
      (get-in seat [:seat/player :player/controller :user/uuid])))

(defn ^:private go-fish-turn-player-id
  "Mirrors events.cljs's go-fish-turn-player -- the currently-active
   seat's player id at `minigame`, or nil."
  [minigame]
  (let [seats (sort-by :seat/order (:minigame/seats minigame))
        players (mapv (comp :db/id :seat/player) seats)
        players-by-id (into {} (map (juxt (comp :db/id :seat/player) :seat/player)) seats)
        active? (fn [id] (:player/active (players-by-id id)))
        idx (turn-order/valid-turn-index players active? (or (:minigame/turn-index minigame) 0))]
    (if idx (nth players idx))))

(defn ^:private selected-state
  "Derived state the hand/ask detail view needs for one selected
   session."
  [minigame uuid host connected]
  (let [seats (sort-by :seat/order (:minigame/seats minigame))
        turn-players (mapv (comp :db/id :seat/player) seats)
        seat-by-player-id (into {} (map (juxt (comp :db/id :seat/player) identity)) seats)
        players-by-id (into {} (map (juxt (comp :db/id :seat/player) :seat/player)) seats)
        cards (:deck/cards (:minigame/deck minigame))
        neutral? (:minigame/neutral-authority? minigame)
        default-authority (and host (not neutral?))
        ;; Separate from default-authority above: whether the viewer
        ;; may ASK on behalf of the current turn player is a turn-
        ;; continuity concern, not a visibility one -- it deliberately
        ;; is NOT suppressed by :minigame/neutral-authority?, mirroring
        ;; go-fish-authorized-for-turn? in events.cljs (stays host-
        ;; fallback regardless of neutral mode, so an unassigned seat's
        ;; turn is never unreachable by anyone). Conflating this with
        ;; hand-visibility authority would soft-lock the game the
        ;; instant an unassigned player's turn comes up under Go Fish's
        ;; own auto-neutral default.
        turn-authority host
        active? (fn [id] (:player/active (players-by-id id)))
        current-index (turn-order/valid-turn-index turn-players active? (or (:minigame/turn-index minigame) 0))
        current-player-id (if current-index (nth turn-players current-index))
        next-index (if (seq turn-players)
                     (turn-order/valid-turn-index
                      turn-players active? (mod (inc (or current-index 0)) (count turn-players))))
        next-player-id (if next-index (nth turn-players next-index))
        draw-count (count (filter (comp #{:draw} :card/location) cards))
        hand-count (count (filter (comp #{:hand} :card/location) cards))
        current-seat (seat-by-player-id current-player-id)]
    {:seats seats
     :turn-players turn-players
     :current-index current-index
     :current-player-id current-player-id
     :next-player-id next-player-id
     :scores (or (:minigame/scores minigame) {})
     :cards cards
     :draw-count draw-count
     :finished? (and (seq turn-players) (zero? draw-count) (zero? hand-count))
     :default-authority default-authority
     :my-turn? (and current-player-id
                    (card-hand/hand-authorized-with?
                     uuid turn-authority connected (if current-seat (seat-controller-uuid current-seat))))}))

(defui ^:private ask-controls
  [{:keys [minigame-id asker-id targets rank-options ask-anyone? next-player dispatch]}]
  (let [[target set-target] (uix/use-state (:db/id (first targets)))
        [rank set-rank] (uix/use-state (first rank-options))]
    ($ :.go-fish-ask
      ($ :.go-fish-ask-row
        ($ :label "Ask")
        (if ask-anyone?
          ($ :select.go-fish-ask-target
            {:value (or target "") :on-change #(set-target (js/Number (.. % -target -value)))}
            (for [entity targets]
              ($ :option {:key (:db/id entity) :value (:db/id entity)} (:player/name entity))))
          ($ :span.go-fish-ask-target-fixed (:player/name next-player)))
        ($ :label "for")
        ($ :select.go-fish-ask-rank
          {:value (or rank "") :on-change #(set-rank (keyword (.. % -target -value)))}
          (for [r rank-options]
            ($ :option {:key r :value (name r)} (rank-short r)))))
      ($ :button.button.button-neutral
        {:type "button"
         :disabled (or (nil? rank) (nil? (if ask-anyone? target (:db/id next-player))))
         :on-click #(dispatch :go-fish/ask minigame-id asker-id (if ask-anyone? target (:db/id next-player)) rank)}
        "Ask"))))

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        publish (hooks/use-publish)
        result (hooks/use-query query [:db/ident :root])
        {uuid :user/uuid host :user/host viewing :user/minigame-viewing
         {scene :camera/scene} :user/camera} (:root/user result)
        sessions (go-fish-sessions scene)
        players (:root/players result)
        players-by-id (into {} (map (juxt :db/id identity)) players)
        connected (into #{} (map :user/uuid) (:session/conns (:root/session result)))
        enabled-elements (:game-type/enabled-elements (:scene/game-type scene))
        ask-anyone? (contains? enabled-elements :go-fish/ask-anyone)
        book-scoring? (contains? enabled-elements :go-fish/book-scoring)
        selected (or (first (filter (comp #{(:db/id viewing)} :db/id) sessions)) (first sessions))
        {:keys [seats current-index current-player-id next-player-id scores cards draw-count
                finished? default-authority my-turn?]}
        (if selected (selected-state selected uuid host connected) {})]
    ($ :.form-go-fish
      ($ :header ($ :h2 "Go Fish"))
      ($ minigame/session-list
        {:minigames sessions
         :selected-id (:db/id selected)
         :players-by-id players-by-id
         :turn-player-id-of go-fish-turn-player-id
         :dispatch dispatch})
      (cond
        (not selected)
        ($ :.form-notice
          "Deal 6 cards to each participant you pick below, from a fresh
           36-card, 9-rank deck. On your turn, ask another player for a
           rank you already hold -- if they have it, they hand over
           every matching card; otherwise you draw from the pile. Lay
           down a matched set whenever you complete one. Several tables
           can run at once, each with its own participants.")

        finished?
        (let [winner-ids (turn-order/winners scores)]
          ($ :<>
            ($ :.form-notice
              (if (= (count winner-ids) 1)
                (str (:player/name (players-by-id (first winner-ids))) " wins!")
                (str "Tied: " (join ", " (map (comp :player/name players-by-id) winner-ids)))))
            ($ :ul.go-fish-scores
              (for [seat seats]
                (let [id (:db/id (:seat/player seat))]
                  ($ :li.go-fish-score-item {:key id}
                    ($ :span (:player/name (players-by-id id)))
                    ($ :span (get scores id 0))))))))

        :else
        ($ :<>
          ($ :.go-fish-draw-count "Draw pile: " draw-count)
          ($ :ul.card-hands
            (for [[i seat] (map-indexed vector seats)
                  :let [entity (:seat/player seat)
                        id (:db/id entity)
                        authorized? (card-hand/hand-authorized-with?
                                     uuid default-authority connected (seat-controller-uuid seat))]]
              ($ card-hand/hand-view
                {:key id :entity entity :cards cards :authorized? authorized?
                 :current? (= i current-index) :score (get scores id 0)
                 :render-group-extra
                 (fn [rank group]
                   (let [n (go-fish/scoreable-count (count group) book-scoring?)]
                     (if (pos? n)
                       ($ :button.button.button-neutral.go-fish-score-button
                         {:type "button" :on-click #(dispatch :go-fish/score (:db/id selected) id rank)}
                         "Score"))))})))
          (if my-turn?
            (let [asker (players-by-id current-player-id)
                  hand (cards/cards-of-holder cards current-player-id)
                  rank-options (sort-by rank-short (into #{} (map :card/rank) hand))
                  targets (->> (map :db/id seats)
                               (remove #{current-player-id})
                               (map players-by-id)
                               (filter :player/active))]
              ($ ask-controls
                {:minigame-id (:db/id selected)
                 :asker-id current-player-id
                 :targets targets
                 :rank-options rank-options
                 :ask-anyone? ask-anyone?
                 :next-player (players-by-id next-player-id)
                 :dispatch dispatch})))))
      ($ minigame/new-session-form
        {:players players
         :submit-label "Start table"
         :on-submit
         (fn [ids]
           (if host
             (dispatch :go-fish/start ids)
             (publish :minigame/create-request :go-fish ids)))}))))

(defui ^:memo actions []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        {uuid :user/uuid host :user/host viewing :user/minigame-viewing
         {scene :camera/scene} :user/camera} (:root/user result)
        sessions (go-fish-sessions scene)
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
