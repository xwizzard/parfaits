(ns ogres.app.component.panel-memory
  "The 'Memory' example game panel/tab -- a minimal UI over events.cljs's
   :memory/* methods, ogres.app.memory's pure dealing logic, and
   ogres.app.turn-order's shared turn-cycle/winner logic. Visible to
   host and guest alike (unlike the host-only Roster/
   Scene tabs), since seeing turn/score state is exactly what every
   connected participant needs -- but the host-only actions (Start/End)
   are gated inline within this panel rather than via tab visibility.

   Deliberately minimal, matching this session's other example-game
   passes (the card/deck system's Decks panel): a turn-order/score list
   here, a Start/Resolve/End button in the footer. Flipping a card
   itself happens on the canvas (the existing hide/reveal control,
   Memory-aware -- see scene_context_menu.cljs), not from this panel."
  (:require [clojure.string :refer [join]]
            [ogres.app.component :refer [icon]]
            [ogres.app.hooks :as hooks]
            [ogres.app.turn-order :as turn-order]
            [uix.core :as uix :refer [defui $]]))

(def ^:private query
  [{:root/user
    [:user/host
     {:user/camera
      [{:camera/scene
        [[:scene/memory-players :default nil]
         [:scene/memory-turn-index :default nil]
         [:scene/memory-scores :default nil]
         {:scene/props
          [[:object/hidden :default false]
           [:object/variables :default nil]]}]}]}]}
   {:root/players [:db/id :player/name :player/color :player/kind :player/active]}])

(defn ^:private memory-state
  "Derived Memory-game state both `panel` and `actions` need, pulled
   once per render via the shared `query` above."
  [result]
  (let [host (:user/host (:root/user result))
        players-by-id (into {} (map (juxt :db/id identity)) (:root/players result))
        scene (:camera/scene (:user/camera (:root/user result)))
        {turn-players :scene/memory-players
         turn-index :scene/memory-turn-index
         scores :scene/memory-scores
         props :scene/props} scene
        cards (filter (comp :memory/value :object/variables) props)
        face-up (remove :object/hidden cards)]
    {:host host
     :players-by-id players-by-id
     :turn-players turn-players
     :turn-index turn-index
     :current-index (turn-order/valid-turn-index
                      turn-players
                      (fn [id] (:player/active (players-by-id id)))
                      (or turn-index 0))
     :scores (or scores {})
     :started? (seq turn-players)
     :finished? (and (seq turn-players) (empty? cards))
     :face-up-count (count face-up)}))

(defui ^:memo panel []
  (let [result (hooks/use-query query [:db/ident :root])
        {:keys [players-by-id turn-players current-index scores started? finished?]}
        (memory-state result)]
    ($ :.form-memory
      ($ :header ($ :h2 "Memory"))
      (cond
        (not started?)
        ($ :.form-notice
          "Deal 22 matching pairs (44 cards) face-down onto this scene as
           a shared Memory game, using whichever roster players are
           currently active. Any connected player may flip cards on
           their own turn -- add or bench participants from the Players
           tab before starting.")

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
              ($ :span.memory-turn-score (get scores id 0)))))))))

(defui ^:memo actions []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        {:keys [host started? finished? face-up-count]} (memory-state result)]
    ($ :<>
      (cond
        (not started?)
        (if host
          ($ :button.button.button-neutral
            {:type "button" :on-click #(dispatch :memory/start)}
            ($ icon {:name "card-front" :size 16})
            "Start Memory Game"))

        finished?
        (if host
          ($ :button.button.button-neutral
            {:type "button" :on-click #(dispatch :memory/end)}
            "New Game"))

        :else
        ($ :<>
          (if (= face-up-count 2)
            ($ :button.button.button-neutral
              {:type "button" :on-click #(dispatch :memory/resolve)}
              "Resolve Turn"))
          (if host
            ($ :button.button.button-danger
              {:type "button" :on-click #(dispatch :memory/end)}
              ($ icon {:name "trash3-fill" :size 16})
              "End Game")))))))
