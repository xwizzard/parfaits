(ns ogres.app.component.card-hand
  "Shared 'private hand' UI -- a per-seat row (color/name/optional
   score/count) showing either a viewer's own real, rank-grouped cards
   or, when they're not authorized to see them, a face-down count only
   -- extracted once a second private-hand game (Old Maid) needed the
   identical piece Go Fish's panel first built. Mirrors the role
   game_type/widgets.cljs already plays for per-token widgets shared
   across game-type modules (form-hp/status-checklist), just for this
   'private hand' shape instead.

   Authorization is checked per seat via player/authority? (the same
   primitive scene_objects.cljs/panel_initiative.cljs already use for
   canvas-object/token visibility, here pointed at a roster player's
   own controller) -- callers decide what 'default' authority means for
   their own game (see hand-authorized?'s docstring), since that's
   where a real difference can exist (e.g. turn-continuity vs. hand-
   visibility authority, see panel_go_fish.cljs)."
  (:require [ogres.app.component :refer [icon]]
            [ogres.app.component.card-pile :refer [rank-short]]
            [ogres.app.player :as player]
            [uix.core :refer [defui $]]))

(defn hand-authorized-with?
  "Whether the local viewer (`uuid`) may see the real hand of a seat
   whose effective controller is `controller-uuid` -- `default-
   authority` is the caller's own choice of fallback for an
   unassigned/disconnected controller (see player/authority?). The
   uuid-taking sibling of hand-authorized? below, for a caller (e.g.
   a mini-game session's own per-seat :seat/controller override, see
   events.cljs's minigame-controller-uuid) that resolves controller
   authority from somewhere other than the roster player's own
   :player/controller directly."
  [uuid default-authority connected controller-uuid]
  (player/authority? uuid default-authority connected controller-uuid))

(defn hand-authorized?
  "Whether `entity` (a roster player, pulled with {:player/controller
   [:user/uuid]}) is one the local viewer may see the real hand of --
   `default-authority` is the caller's own choice of fallback for an
   unassigned/disconnected-controller seat (see player/authority?)."
  [uuid default-authority connected entity]
  (hand-authorized-with? uuid default-authority connected (get-in entity [:player/controller :user/uuid])))

(defui ^:private hand-card [{:keys [card on-click playable?]}]
  ($ :.card-hand-card
    {:data-playable (boolean playable?)
     :on-click (if on-click #(on-click card))}
    (if-let [abbrev (rank-short (:card/rank card))]
      ($ :.card-hand-card-rank abbrev))
    ($ icon {:name (:card/icon card) :size 14 :color (:card/icon-color card)})))

(defui ^:private hand-card-back [{:keys [card on-click selectable?]}]
  ($ :.card-hand-card.card-hand-card-back
    {:data-selectable (boolean selectable?)
     :on-click (if on-click #(on-click card))}))

(defui hand-view
  "One seat's hand row. `cards` is the WHOLE deck's cards (filtered
   internally to `entity`'s own holder-id AND :card/location :hand --
   a card moved to :scored/:discard/:draw keeps :card/holder set
   whenever a game credits someone for it there too, e.g. Rummy's
   shared table sets, so holder alone isn't enough to mean 'still in
   this seat's hand') -- `authorized?` decides between real, rank-
   grouped cards and a face-down placeholder.
   `score`, when given, renders as a small numeric badge (omit for a
   game with nothing numeric to show, e.g. Old Maid).
   `render-group-extra`, when given, is called as (fn [rank group]
   hiccup-or-nil) for each rank-group's trailing content -- Go Fish
   passes a per-rank 'Score' button; a game with no per-rank action at
   all (Old Maid, whose discarding is automatic) simply omits it,
   proving this shared piece doesn't leak Go-Fish-specific assumptions.
   `on-card-click`/`card-playable?`, when given, wire up per-CARD (not
   per-group) interaction instead -- Crazy 8s' need, since which of
   several same-rank cards is legal depends on suit too, a finer grain
   than render-group-extra's whole-group callback offers. Go Fish and
   Old Maid omit both, proving this doesn't leak Crazy-8s assumptions
   either.

   `show-backs?`, when true, renders an UNAUTHORIZED hand as one
   face-down placeholder per real card (stable-ordered by
   :card/position) instead of the plain 'N cards' summary -- each
   placeholder still corresponds to one specific, real (if invisible
   to this viewer) card entity. `back-card-click`/
   `back-card-selectable?` (same per-card shape as their face-up
   counterparts) wire up clicking one, e.g. Old Maid's 'pick a card
   from your neighbor's hand to draw' -- letting a player choose WHICH
   position to draw from, instead of the game silently picking one at
   random for them, without ever revealing what any placeholder holds
   until it's actually drawn. Go Fish and Crazy 8s omit all three,
   proving this doesn't leak Old-Maid-specific assumptions either."
  [{:keys [entity cards authorized? current? score render-group-extra
           on-card-click card-playable? show-backs? back-card-click back-card-selectable?]}]
  (let [id (:db/id entity)
        hand (filter (fn [c] (and (= (:card/location c) :hand) (= (get-in c [:card/holder :db/id]) id))) cards)
        by-rank (group-by :card/rank hand)]
    ($ :li.card-hand-item
      {:key id :data-current current? :data-active (boolean (:player/active entity))}
      ($ :.card-hand-header
        ($ :span.card-hand-color {:data-color (:player/color entity)})
        ($ :span.card-hand-name (:player/name entity))
        (if score ($ :span.card-hand-score score))
        ($ :span.card-hand-count (count hand)))
      (cond
        authorized?
        ($ :ul.card-hand-cards
          (for [[rank group] (sort-by (comp rank-short key) by-rank)]
            ($ :li.card-hand-group {:key rank}
              (for [card group]
                ($ hand-card
                  {:key (:db/id card) :card card :on-click on-card-click
                   :playable? (if card-playable? (card-playable? card))}))
              (if render-group-extra (render-group-extra rank group)))))

        show-backs?
        ($ :ul.card-hand-cards
          (for [card (sort-by :card/position hand)]
            ($ hand-card-back
              {:key (:db/id card) :card card :on-click back-card-click
               :selectable? (if back-card-selectable? (back-card-selectable? card))})))

        :else
        ($ :.card-hand-back (str (count hand) " card" (if (not= (count hand) 1) "s")))))))
