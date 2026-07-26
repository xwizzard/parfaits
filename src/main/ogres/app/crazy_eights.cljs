(ns ogres.app.crazy-eights
  "Pure logic for the 'Crazy 8s' example game -- a demonstration of the
   generic card/deck system's face-up :card/location :discard pile
   (unused by Memory/Go Fish/Old Maid until now) and per-card, not
   per-rank or per-hand, legality. No DataScript, no UI; mirrors the
   role ogres.app.memory/ogres.app.go-fish/ogres.app.old-maid play for
   their own games. See events.cljs's :crazy-eights/* methods and
   component/panel_crazy_eights.cljs.")

(defn playable?
  "Whether `card` may legally be played onto `top` (the discard pile's
   current face-up card) right now. An 8 is wild -- always legal, any
   time, not just as a last resort. Otherwise `card` must match `top`
   by rank, or by suit -- the suit to match is `declared-suit` when
   `top` itself is an 8 (its player named a suit when they played it,
   since an 8 has no :card/suit of its own to match against), or
   `top`'s own :card/suit otherwise."
  [card top declared-suit]
  (boolean
   (or (= (:card/rank card) :eight)
       (if (= (:card/rank top) :eight)
         (= (:card/suit card) declared-suit)
         (or (= (:card/rank card) (:card/rank top))
             (= (:card/suit card) (:card/suit top)))))))

(defn playable-cards
  "The subset of `hand` currently legal to play onto `top` -- see
   playable?."
  [hand top declared-suit]
  (filter #(playable? % top declared-suit) hand))
