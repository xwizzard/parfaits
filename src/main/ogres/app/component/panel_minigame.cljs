(ns ogres.app.component.panel-minigame
  "Shared UI for the mini-game session list/creator/controller-assignment
   -- extracted so the other example games can reuse it once they're
   ported onto the same :scene/minigames scaffolding Old Maid
   prototypes first (see events.cljs's 'Mini-game sessions' section
   and component/panel_old_maid.cljs, its first and so far only
   caller). Deliberately data-in/dispatch-out, the same shape card_
   hand.cljs's hand-view already establishes -- no query of its own,
   so a caller panel decides exactly what it pulls and how."
  (:require [clojure.string :as string]
            [ogres.app.component :refer [icon]]
            [ogres.app.hooks :as hooks]
            [uix.core :as uix :refer [defui $]]))

(def ^:private kind-labels
  {:human "Player" :npc "NPC"})

(defn ^:private conn-label
  [conn]
  (or (:user/label conn) (str "Guest (" (:user/color conn) ")")))

(defn ^:private seat-summary
  "One seat's display text within a session row's participant list --
   the roster player's name, with a trailing marker if it's currently
   their turn."
  [players-by-id turn-player-id seat]
  (let [player-id (get-in seat [:seat/player :db/id])]
    (cond-> (:player/name (players-by-id player-id) "")
      (= player-id turn-player-id) (str " *"))))

(defui ^:private session-row [{:keys [minigame selected? players-by-id turn-player-id dispatch]}]
  (let [{id :db/id label :minigame/label seats :minigame/seats} minigame
        seats (sort-by :seat/order seats)]
    ($ :li.minigame-session-row
      ($ :label.radio
        ($ :input
          {:type "radio"
           :name "minigame-viewing"
           :checked (boolean selected?)
           :on-change #(dispatch :user/view-minigame id)})
        ($ :span.minigame-session-label label)
        ($ :span.minigame-session-players
          (string/join ", " (map (partial seat-summary players-by-id turn-player-id) seats))))
      ($ :button.minigame-session-remove
        {:type "button"
         :aria-label (str "End " label)
         :title "End table"
         :on-click #(dispatch :minigame/remove id)}
        ($ icon {:name "trash3-fill" :size 16})))))

(defui session-list
  "The scene's sessions of one game kind, as a radio-selectable list
   (see panel_game_type_builder.cljs's template-row, the pattern this
   mirrors) -- `minigames` is that kind's :scene/minigames subset,
   `selected-id` the currently-viewed session's :db/id (or nil),
   `turn-player-id-of` a (fn [minigame]) resolving whose turn it
   currently is there, for the trailing '*' marker."
  [{:keys [minigames selected-id players-by-id turn-player-id-of dispatch]}]
  (if (seq minigames)
    ($ :ul.minigame-session-list
      (for [minigame minigames]
        ($ session-row
          {:key (:db/id minigame)
           :minigame minigame
           :selected? (= (:db/id minigame) selected-id)
           :players-by-id players-by-id
           :turn-player-id (turn-player-id-of minigame)
           :dispatch dispatch})))
    ($ :.form-notice "No tables yet -- start one below.")))

(defui new-session-form
  "A participant checkbox list, grouped by roster kind (human/NPC),
   plus a 'Start table' submit -- `players` is the whole roster
   (:root/players), `on-submit` is called with the vector of selected
   player ids. Requires at least 2 selected participants, mirroring
   :old-maid/start's own minimum (checked there too, since this UI
   gate alone isn't authoritative -- see its docstring)."
  [{:keys [players on-submit submit-label]}]
  (let [[selected set-selected] (uix/use-state #{})
        toggle (fn [id] (set-selected (fn [s] (if (contains? s id) (disj s id) (conj s id)))))]
    ($ :form.minigame-new-session
      {:on-submit
       (fn [event]
         (.preventDefault event)
         (if (>= (count selected) 2)
           (do (on-submit (vec selected))
               (set-selected #{}))))}
      (for [[kind entities] (group-by :player/kind players)]
        ($ :fieldset.minigame-new-session-group {:key kind}
          ($ :legend (kind-labels kind))
          (for [entity entities]
            ($ :label.checkbox {:key (:db/id entity)}
              ($ :input
                {:type "checkbox"
                 :checked (contains? selected (:db/id entity))
                 :on-change #(toggle (:db/id entity))})
              ($ icon {:name "check" :size 20})
              (:player/name entity)))))
      ($ :button.button.button-neutral
        {:type "submit" :disabled (< (count selected) 2)}
        ($ icon {:name "plus" :size 16})
        (or submit-label "Start table")))))

(defui controller-select
  "A <select> assigning which currently-connected participant controls
   `seat` for the duration of this one session -- \"Default\" (the
   default, clears the :seat/controller override, falling back to the
   roster player's own :player/controller) or one of the currently-
   connected users (see events.cljs's :minigame/set-controller). The
   per-session sibling of panel_roster.cljs's own controller-picker."
  [{:keys [seat conns minigame-id dispatch]}]
  (let [player-id (get-in seat [:seat/player :db/id])
        controller-uuid (get-in seat [:seat/controller :user/uuid])]
    ($ :select.minigame-seat-controller
      {:value (or controller-uuid "")
       :on-change
       (fn [event]
         (let [v (.. event -target -value)]
           (dispatch :minigame/set-controller minigame-id player-id (if (seq v) v nil))))}
      ($ :option {:value ""} "Default")
      (for [conn conns]
        ($ :option {:key (:user/uuid conn) :value (:user/uuid conn)} (conn-label conn))))))
