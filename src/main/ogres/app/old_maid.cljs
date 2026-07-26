(ns ogres.app.old-maid
  "Pure logic for the 'Old Maid' example game -- a demonstration of the
   generic card/deck system's :card/holder-based hands, dealing the
   *entire* deck unevenly, and a blind, targetless draw, not part of
   the generic engine itself. No DataScript, no UI; mirrors the role
   ogres.app.memory/ogres.app.go-fish play for their own games. See
   events.cljs's :old-maid/* methods and
   component/panel_old_maid.cljs. Turn-cycle logic lives in
   ogres.app.turn-order, and card-filtering helpers in ogres.app.cards
   -- both shared with Go Fish, nothing here duplicates either.")

(defn pairs-to-discard
  "Every card in `hand` that's part of a complete same-rank pair --
   each rank rounds DOWN to the nearest even count, so a 4-of-a-kind
   (all 4 suits of one rank) correctly discards as 2 separate pairs, a
   3-of-a-kind discards exactly 1 pair and leaves 1 card in hand, and
   the lone reskinned queen (no rank-mate left at all, see
   game_type.games.old-maid/deck-definitions) is never included."
  [hand]
  (mapcat (fn [[_ cards]] (take (* 2 (quot (count cards) 2)) cards))
          (group-by :card/rank hand)))
