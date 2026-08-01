(ns ogres.app.component.panel-memory
  "The 'Memory' example game panel/tab -- a minimal UI over events.cljs's
   :memory/* methods, ogres.app.memory's pure dealing logic, and
   ogres.app.turn-order's shared turn-cycle/winner logic. Visible to
   host and guest alike (unlike the host-only Roster/
   Scene tabs), since seeing turn/score state is exactly what every
   connected participant needs -- but the host-only actions (Start/End)
   are gated inline within this panel rather than via tab visibility.

   The second game ported onto the generic, nested mini-game session
   scaffolding (see events.cljs's 'Mini-game sessions' section and
   component/panel_minigame.cljs, whose session-list/new-session-form
   this panel composes, same as panel_old_maid.cljs) -- several
   independent Memory tables, each seated by an arbitrary subset of the
   roster, can run at once on one scene.

   Deliberately minimal, matching this session's other example-game
   passes (the card/deck system's Decks panel): a turn-order/score list
   here, a Start/Resolve/End button in the footer. Flipping a card
   itself happens on the canvas (the existing hide/reveal control,
   Memory-aware -- see scene_context_menu.cljs), not from this panel --
   :memory/flip resolves its own session from the card it's given (see
   its docstring), so this panel's own 'which session is selected'
   state doesn't need to reach the canvas at all."
  (:require [clojure.string :refer [join]]
            [ogres.app.component :refer [icon]]
            [ogres.app.component.panel-minigame :as minigame]
            [ogres.app.hooks :as hooks]
            [ogres.app.memory :as memory]
            [ogres.app.turn-order :as turn-order]
            [uix.core :as uix :refer [defui $]]))

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
           :memory/difficulty
           [:minigame/turn-index :default 0]
           [:minigame/scores :default nil]
           {:minigame/seats
            [:db/id [:seat/order :default 0] {:seat/player [:db/id]}]}
           {:minigame/cards
            [:db/id [:memory/face-up? :default false] :memory/value]}]}]}]}]}
   {:root/players [:db/id :player/name :player/color :player/kind [:player/active :default true]]}])

(defn ^:private memory-sessions [scene]
  (filter (comp #{:memory} :minigame/kind) (:scene/minigames scene)))

(defn ^:private memory-state
  "Derived Memory-game state both `panel` and `actions` need, pulled
   once per render via the shared `query` above."
  [result]
  (let [host (:user/host (:root/user result))
        players-by-id (into {} (map (juxt :db/id identity)) (:root/players result))
        viewing (:user/minigame-viewing (:root/user result))
        scene (:camera/scene (:user/camera (:root/user result)))
        sessions (memory-sessions scene)
        selected (or (first (filter (comp #{(:db/id viewing)} :db/id) sessions)) (first sessions))
        turn-players (mapv (comp :db/id :seat/player) (sort-by :seat/order (:minigame/seats selected)))
        turn-index (:minigame/turn-index selected)
        scores (:minigame/scores selected)
        cards (:minigame/cards selected)
        face-up (filter :memory/face-up? cards)]
    {:host host
     :players-by-id players-by-id
     :sessions sessions
     :selected selected
     :turn-players turn-players
     :current-index (turn-order/valid-turn-index
                      turn-players
                      (fn [id] (:player/active (players-by-id id)))
                      (or turn-index 0))
     :scores (or scores {})
     ;; A table is placed first and dealt into later, so "has seats" --
     ;; not "exists" -- is what separates an empty frame waiting to be
     ;; positioned from a game in progress.
     :started? (boolean (seq turn-players))
     :finished? (and selected (seq turn-players) (empty? cards))
     :face-up-count (count face-up)}))

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        publish (hooks/use-publish)
        result (hooks/use-query query [:db/ident :root])
        host (:user/host (:root/user result))
        {:keys [players-by-id sessions selected turn-players current-index scores finished? started?]}
        (memory-state result)
        ;; The size the NEXT table gets placed at. Once a table exists
        ;; and is still empty, the select drives its stored size instead.
        [pending-difficulty set-pending-difficulty]
        (uix/use-state memory/default-difficulty)]
    ($ :.form-memory
      ($ :header ($ :h2 "Memory"))
      ($ minigame/session-list
        {:minigames sessions
         :selected-id (:db/id selected)
         :players-by-id players-by-id
         :turn-player-id-of
         (fn [mg]
           (let [players (mapv (comp :db/id :seat/player) (sort-by :seat/order (:minigame/seats mg)))
                 idx (turn-order/valid-turn-index
                      players (fn [id] (:player/active (players-by-id id))) (or (:minigame/turn-index mg) 0))]
             (if idx (nth players idx))))
         :dispatch dispatch})
      (cond
        (not selected)
        ($ :.form-notice
          "Place an empty Memory table on this scene, drag and resize it
           to fit, then deal a standard 52-card deck face-down onto it.
           Pairs match on rank and colour, so one deck makes 26 pairs.
           Any connected player may flip cards on their own turn, and
           several tables can run at once, each with its own
           participants.")

        finished?
        (let [winner-ids (turn-order/winners scores)]
          ($ :<>
            ($ :.form-notice
              (if (= (count winner-ids) 1)
                (str (:player/name (players-by-id (first winner-ids))) " wins!")
                (str "Tied: " (join ", " (map (comp :player/name players-by-id) winner-ids)))))
            ($ :ul.memory-scores
              (for [id turn-players]
                ($ :li.memory-score-item {:key id}
                  ($ :span (:player/name (players-by-id id)))
                  ($ :span (get scores id 0)))))))

        (not started?)
        ($ :.form-notice
          "Empty table placed. Select it on the scene to drag it into
           position and resize it from its corners -- its dimensions
           lock once the cards are dealt. Then pick who is playing and
           start the table below.")

        :else
        ($ :ul.memory-turn-order
          (for [[i id] (map-indexed vector turn-players)
                :let [entity (players-by-id id)]]
            ($ :li.memory-turn-item
              {:key id
               :data-current (= i current-index)
               :data-active (boolean (:player/active entity))
               :data-color (:player/color entity)}
              ($ :span.memory-turn-color)
              ($ :span.memory-turn-name (:player/name entity))
              ($ :span.memory-turn-score (get scores id 0))))))
      ;; Size is settled before the deal, alongside position and scale --
      ;; it decides the felt's own footprint, so it locks with everything
      ;; else the moment cards land (see :memory/change-difficulty).
      (if (and host (or (not selected) (not started?)))
        ($ :label.memory-difficulty
          ($ :span "Size")
          ($ :select
            {:value (str (if (and selected (not started?))
                           (memory/difficulty selected)
                           pending-difficulty))
             :on-change
             (fn [event]
               (let [level (js/parseInt (.. event -target -value) 10)]
                 (set-pending-difficulty level)
                 (if (and selected (not started?))
                   (dispatch :memory/change-difficulty (:db/id selected) level))))}
            (for [level (range 0 (inc memory/max-difficulty))
                  :let [n (memory/deck-size level)]]
              ($ :option {:key level :value (str level)}
                (str n " cards / " (quot n 2) " pairs"
                     (cond (zero? level) " -- aces and faces"
                           (= level memory/max-difficulty) " -- everything"
                           :else "")))))))
      ;; Placing mints entities, so it stays with the host for the same
      ;; reason starting is relayed to them (see :minigame/create-request)
      ;; -- there is no cross-peer entity-id partitioning. The host lays
      ;; out the board; any participant can then start a game on it.
      (if host
        ($ :button.button.button-neutral
          {:type "button" :on-click #(dispatch :memory/place-table pending-difficulty)}
          ($ icon {:name "plus-circle-fill" :size 16})
          "Place table"))
      (if (and selected (not started?))
        ($ minigame/new-session-form
          {:players (:root/players result)
           :submit-label "Start table"
           ;; Memory alone is a real way to play it -- there is nothing
           ;; hidden from an opponent, just a board and your own recall.
           :min-players 1
           :on-submit
           (fn [ids]
             (if host
               (dispatch :memory/start (:db/id selected) ids)
               (publish :minigame/create-request :memory ids (:db/id selected))))})))))

(defui ^:memo actions []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        {:keys [selected finished? face-up-count]} (memory-state result)]
    (if selected
      (cond
        finished?
        ($ :button.button.button-neutral
          {:type "button" :on-click #(dispatch :minigame/remove (:db/id selected))}
          "New Game")

        :else
        ($ :<>
          (if (= face-up-count 2)
            ($ :button.button.button-neutral
              {:type "button" :on-click #(dispatch :memory/resolve (:db/id selected))}
              "Resolve Turn"))
          ($ :button.button.button-danger
            {:type "button" :on-click #(dispatch :minigame/remove (:db/id selected))}
            ($ icon {:name "trash3-fill" :size 16})
            "End Table"))))))
