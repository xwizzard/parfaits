(ns ogres.app.cards
  "Pure card/deck logic -- shuffling, rank-value resolution, and reshuffle
   detection. No DataScript, no UI; mirrors the role ogres.app.geom/vec
   play for grid math. See events.cljs's :deck/* methods for the
   transactional layer that uses these, and game_type/core_decks.cljs for
   the deck/card data these operate on.")

(defn shuffle-positions
  "Returns a map of {id position} assigning every id in `ids` a distinct
   position -- a dense 0..n-1 sequence, permuted -- so 'top of pile' is
   always whichever id holds the max position for its pile. A fresh call
   always reshuffles from scratch; it doesn't matter what positions (if
   any) the ids previously held."
  [ids]
  (zipmap (shuffle (vec ids)) (range (count ids))))

(def ^:private rank-order
  "Canonical, ruleset-independent low-to-high order for every rank except
   :ace, whose value depends on the ruleset -- see rank-value. Index 0
   (:two) is value 2, so a rank's value is always (+ 2 its index here),
   regardless of ace-high?/low."
  [:two :three :four :five :six :seven :eight :nine :ten :jack :queen :king])

(def ^:private rank-set (set rank-order))

(defn rank-value
  "Resolves a card rank to a comparable number, given a ruleset (currently
   just {:ace-high? bool}, default true) -- nil for a rank with no defined
   numeric value (e.g. :joker). This is the ONLY place rank ambiguity is
   resolved: a card's stored :card/rank never changes based on ruleset,
   since which interpretation is correct is a per-game rule, not a
   property of the card itself. Every rank besides :ace has a fixed value
   (2-13) regardless of the ruleset -- ace-high?/low only changes whether
   :ace itself resolves to 14 or 1. Not used by :deck/create, :deck/draw,
   :deck/discard, :deck/deal, :deck/reset, or :deck/shuffle at all --
   those only ever move cards by pile position, never by rank -- this
   exists for a future game rule (or a comparison feature like a
   War-style 'highest card wins') to call."
  [rank {:keys [ace-high?] :or {ace-high? true}}]
  (cond
    (= rank :ace) (if ace-high? 14 1)
    (contains? rank-set rank) (+ 2 (.indexOf rank-order rank))
    :else nil))

(defn needs-reshuffle?
  "True when a deck's draw pile is empty and its discard pile has cards to
   reclaim. This is the universal default rule every card game
   effectively uses; it's a standalone predicate (not inlined into
   :deck/draw) specifically so a later Gloomhaven-specific rule (reshuffle
   triggered early by a flagged card, not just an empty pile) has a clean
   seam to swap into instead of rewriting :deck/draw's transaction logic."
  [draw-cards discard-cards]
  (boolean (and (empty? draw-cards) (seq discard-cards))))
