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
           [:minigame/turn-index :default 0]
           [:minigame/scores :default nil]
           {:minigame/seats
            [:db/id [:seat/order :default 0] {:seat/player [:db/id]}]}
           {:minigame/props
            [:db/id [:object/hidden :default false] [:object/variables :default nil]]}]}]}]}]}
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
        cards (:minigame/props selected)
        face-up (remove :object/hidden cards)]
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
     :finished? (and selected (seq turn-players) (empty? cards))
     :face-up-count (count face-up)}))

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        publish (hooks/use-publish)
        result (hooks/use-query query [:db/ident :root])
        host (:user/host (:root/user result))
        {:keys [players-by-id sessions selected turn-players current-index scores finished?]}
        (memory-state result)]
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
          "Deal 22 matching pairs (44 cards) face-down onto this scene as
           a shared Memory table, seated by whichever subset of the
           roster you pick below. Any connected player may flip cards on
           their own turn. Several tables can run at once, each with its
           own participants.")

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
      ($ minigame/new-session-form
        {:players (:root/players result)
         :submit-label "Start table"
         :on-submit
         (fn [ids]
           (if host
             (dispatch :memory/start ids)
             (publish :minigame/create-request :memory ids)))}))))

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
