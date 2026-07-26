(ns ogres.app.component.panel-go-fish
  "The 'Go Fish' example game panel/tab -- a minimal UI over events.cljs's
   :go-fish/* methods, ogres.app.go-fish's pure rank-matching/scoring
   logic, and ogres.app.turn-order's shared turn-cycle/winner logic.
   Visible to host and guest alike, same as Memory's panel.

   The one genuinely new piece Memory's panel never needed: hand
   privacy. Every seat's hand is checked independently via
   player/authority? (the same primitive scene_objects.cljs/
   panel_initiative.cljs already use for canvas-object/token
   visibility, just pointed at a roster player's own controller instead
   of an object's owner) -- authorized hands render real, rank-grouped
   cards with a Score button wherever ogres.app.go-fish/scoreable-count
   currently allows one; unauthorized hands render a face-down count
   only, matching Memory's own hidden-card placeholder convention.
   :scene/neutral-authority? (auto-enabled by :go-fish/start, same as
   Memory) keeps the host from automatically seeing every hand just
   because they're host -- the same 'host is often also a competing
   player' reasoning applies here."
  (:require [clojure.string :refer [join]]
            [ogres.app.component :refer [icon]]
            [ogres.app.go-fish :as go-fish]
            [ogres.app.hooks :as hooks]
            [ogres.app.player :as player]
            [ogres.app.turn-order :as turn-order]
            [uix.core :as uix :refer [defui $]]))

(def ^:private query
  [{:root/user
    [:user/uuid
     :user/host
     {:user/camera
      [{:camera/scene
        [[:scene/go-fish-players :default nil]
         [:scene/go-fish-turn-index :default nil]
         [:scene/go-fish-scores :default nil]
         [:scene/neutral-authority? :default false]
         {:scene/game-type [[:game-type/enabled-elements :default #{}]]}
         {:scene/go-fish-deck
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

(def ^:private rank-short
  {:two "2" :three "3" :four "4" :five "5" :six "6" :seven "7" :eight "8" :nine "9" :ten "10"})

(defn ^:private cards-of-holder [cards holder-id]
  (filter (comp #{holder-id} :db/id :card/holder) cards))

(defn ^:private hand-authorized?
  [uuid default-authority connected entity]
  (player/authority? uuid default-authority connected (get-in entity [:player/controller :user/uuid])))

(defn ^:private go-fish-state
  "Derived Go Fish state `panel`/`actions` both need, pulled once per
   render via the shared `query` above."
  [result]
  (let [{uuid :user/uuid host :user/host
         {scene :camera/scene} :user/camera} (:root/user result)
        players-by-id (into {} (map (juxt :db/id identity)) (:root/players result))
        connected (into #{} (map :user/uuid) (:session/conns (:root/session result)))
        {turn-players :scene/go-fish-players
         turn-index :scene/go-fish-turn-index
         scores :scene/go-fish-scores
         neutral? :scene/neutral-authority?
         enabled-elements :game-type/enabled-elements
         deck :scene/go-fish-deck} scene
        cards (:deck/cards deck)
        default-authority (and host (not neutral?))
        ;; Separate from default-authority above: whether the viewer
        ;; may ASK on behalf of the current turn player is a turn-
        ;; continuity concern, not a visibility one -- it deliberately
        ;; is NOT suppressed by :scene/neutral-authority?, mirroring
        ;; go-fish-authorized-for-turn?/authorized-for-turn? in
        ;; events.cljs (both stay host-fallback regardless of neutral
        ;; mode, so an unassigned seat's turn is never unreachable by
        ;; anyone). Conflating this with hand-visibility authority
        ;; would soft-lock the game the instant an unassigned player's
        ;; turn comes up under Go Fish's own auto-neutral default.
        turn-authority host
        active? (fn [id] (:player/active (players-by-id id)))
        current-index (turn-order/valid-turn-index turn-players active? (or turn-index 0))
        current-player-id (if current-index (nth turn-players current-index))
        next-index (if (seq turn-players)
                     (turn-order/valid-turn-index
                      turn-players active? (mod (inc (or current-index 0)) (count turn-players))))
        next-player-id (if next-index (nth turn-players next-index))
        draw-count (count (filter (comp #{:draw} :card/location) cards))
        hand-count (count (filter (comp #{:hand} :card/location) cards))]
    {:uuid uuid
     :host host
     :connected connected
     :default-authority default-authority
     :players-by-id players-by-id
     :turn-players turn-players
     :current-index current-index
     :current-player-id current-player-id
     :next-player-id next-player-id
     :ask-anyone? (contains? enabled-elements :go-fish/ask-anyone)
     :book-scoring? (contains? enabled-elements :go-fish/book-scoring)
     :scores (or scores {})
     :cards cards
     :draw-count draw-count
     :started? (seq turn-players)
     :finished? (and (seq turn-players) (zero? draw-count) (zero? hand-count))
     :my-turn? (and current-player-id
                    (hand-authorized? uuid turn-authority connected (players-by-id current-player-id)))}))

(defui ^:private hand-card [{:keys [card]}]
  ($ :.go-fish-card
    (if-let [abbrev (rank-short (:card/rank card))]
      ($ :.go-fish-card-rank abbrev))
    ($ icon {:name (:card/icon card) :size 14})))

(defui ^:private ask-controls
  [{:keys [asker-id targets rank-options ask-anyone? next-player dispatch]}]
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
         :on-click #(dispatch :go-fish/ask asker-id (if ask-anyone? target (:db/id next-player)) rank)}
        "Ask"))))

(defui ^:private hand-view
  [{:keys [entity cards authorized? current? dispatch book-scoring? score]}]
  (let [id (:db/id entity)
        hand (cards-of-holder cards id)
        by-rank (group-by :card/rank hand)]
    ($ :li.go-fish-hand-item
      {:key id :data-current current? :data-active (boolean (:player/active entity))}
      ($ :.go-fish-hand-header
        ($ :span.go-fish-hand-color {:data-color (:player/color entity)})
        ($ :span.go-fish-hand-name (:player/name entity))
        ($ :span.go-fish-hand-score score)
        ($ :span.go-fish-hand-count (count hand)))
      (if authorized?
        ($ :ul.go-fish-hand-cards
          (for [[rank group] (sort-by (comp rank-short key) by-rank)]
            ($ :li.go-fish-hand-group {:key rank}
              (for [card group] ($ hand-card {:key (:db/id card) :card card}))
              (let [n (go-fish/scoreable-count (count group) book-scoring?)]
                (if (pos? n)
                  ($ :button.button.button-neutral.go-fish-score-button
                    {:type "button" :on-click #(dispatch :go-fish/score id rank)}
                    "Score"))))))
        ($ :.go-fish-hand-back (str (count hand) " card" (if (not= (count hand) 1) "s")))))))

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        {:keys [uuid host connected default-authority players-by-id turn-players
                current-index current-player-id next-player-id ask-anyone? book-scoring?
                scores cards draw-count started? finished? my-turn?]}
        (go-fish-state result)]
    ($ :.form-go-fish
      ($ :header ($ :h2 "Go Fish"))
      (cond
        (not started?)
        ($ :.form-notice
          "Deal 6 cards to each active roster player from a fresh
           36-card, 9-rank deck. On your turn, ask another player for a
           rank you already hold -- if they have it, they hand over
           every matching card; otherwise you draw from the pile. Lay
           down a matched set whenever you complete one -- add or bench
           participants from the Players tab before starting.")

        finished?
        (let [winner-ids (turn-order/winners scores)]
          ($ :<>
            ($ :.form-notice
              (if (= (count winner-ids) 1)
                (str (:player/name (players-by-id (first winner-ids))) " wins!")
                (str "Tied: " (join ", " (map (comp :player/name players-by-id) winner-ids)))))
            ($ :ul.go-fish-scores
              (for [id turn-players]
                ($ :li.go-fish-score-item {:key id}
                  ($ :span (:player/name (players-by-id id)))
                  ($ :span (get scores id 0)))))))

        :else
        ($ :<>
          ($ :.go-fish-draw-count "Draw pile: " draw-count)
          ($ :ul.go-fish-hands
            (for [[i id] (map-indexed vector turn-players)
                  :let [entity (players-by-id id)
                        authorized? (hand-authorized? uuid default-authority connected entity)]]
              ($ hand-view
                {:key id :entity entity :cards cards :authorized? authorized?
                 :current? (= i current-index) :score (get scores id 0)
                 :dispatch dispatch :book-scoring? book-scoring?})))
          (if my-turn?
            (let [asker (players-by-id current-player-id)
                  hand (cards-of-holder cards current-player-id)
                  rank-options (sort-by rank-short (into #{} (map :card/rank) hand))
                  targets (->> turn-players
                               (remove #{current-player-id})
                               (map players-by-id)
                               (filter :player/active))]
              ($ ask-controls
                {:asker-id current-player-id
                 :targets targets
                 :rank-options rank-options
                 :ask-anyone? ask-anyone?
                 :next-player (players-by-id next-player-id)
                 :dispatch dispatch}))))))))

(defui ^:memo actions []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        {:keys [host started? finished?]} (go-fish-state result)]
    ($ :<>
      (cond
        (not started?)
        (if host
          ($ :button.button.button-neutral
            {:type "button" :on-click #(dispatch :go-fish/start)}
            ($ icon {:name "suit-heart-fill" :size 16})
            "Start Go Fish"))

        finished?
        (if host
          ($ :button.button.button-neutral
            {:type "button" :on-click #(dispatch :go-fish/end)}
            "New Game"))

        :else
        (if host
          ($ :button.button.button-danger
            {:type "button" :on-click #(dispatch :go-fish/end)}
            ($ icon {:name "trash3-fill" :size 16})
            "End Game"))))))
