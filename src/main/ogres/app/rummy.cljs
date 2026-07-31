(ns ogres.app.rummy
  "Pure logic for the 'Rummy' example game -- a demonstration of the
   generic card/deck system's shared, table-wide :card/location
   :scored area (any player may contribute to an existing group, not
   just the player who started it) and a winner decided by a tally on
   a game that ends by hand-emptiness, not by who empties it. No
   DataScript, no UI; mirrors the role ogres.app.go-fish/ogres.app.
   crazy-eights play for their own games. See events.cljs's :rummy/*
   methods and component/panel_rummy.cljs.")

(defn set-scored-count
  "How many cards of one rank are on the table as part of a laid-down
   SET, given every scored card of that rank.

   Cards melded as part of a RUN carry :card/run-meld? and are excluded.
   The shared :scored area holds both kinds indistinguishably otherwise,
   and counting run cards as set progress breaks scoreable-set's
   precondition below in both directions: a legal fresh 3-of-a-kind
   becomes permanently unplayable once one of that rank has gone down in
   somebody's run (scoreable-set answers 0 for an already-scored count of
   1 or 2), and three of a rank arriving via three separate runs lets a
   player 'lay off the 4th' onto a set nobody ever laid down."
  [scored-of-rank]
  (count (remove :card/run-meld? scored-of-rank)))

(defn scoreable-set
  "How many of the player's own `hand-count` cards of one rank would
   be laid down right now, given `already-scored` -- how many of that
   rank are already on the table as part of a SET (see set-scored-count,
   which is how callers must derive this) -- 0 if none eligible. A fresh
   set needs the player to hold >=3 of their own (lays down all they
   hold, 3 or 4 at once); once exactly 3 of a rank are already scored,
   laying off the 4th needs exactly 1 more in hand; a rank already
   fully scored (4) has nothing left to add.

   The 1-or-2 case answering 0 rests on sets only ever being laid down 3
   or 4 at once, so those counts are unreachable for set-melded cards."
  [hand-count already-scored]
  (cond
    (>= already-scored 4) 0
    (pos? already-scored) (if (and (= already-scored 3) (pos? hand-count)) 1 0)
    (>= hand-count 3) (min hand-count 4)
    :else 0))

(def ^:private rank-order
  "Ace-low run ordering (A,2,3...K, no wrap to K-A-2) -- the simplest,
   least surprising default for a run's internal sequence, deliberately
   independent of ogres.app.cards/rank-value's own per-game ace-high?
   ruleset (that resolves a card's numeric VALUE for comparison; this
   is just 'what comes after what' for run detection)."
  [:ace :two :three :four :five :six :seven :eight :nine :ten :jack :queen :king])

(def ^:private rank-value (zipmap rank-order (range (count rank-order))))

(defn ^:private consecutive-groups
  "Splits `sorted-cards` (already ascending by rank-value, one suit)
   into maximal runs of consecutive rank-values, including runs of
   length 1 -- callers filter by length."
  [sorted-cards]
  (reduce (fn [groups card]
            (let [prev (peek (peek groups))]
              (if (and prev (= (rank-value (:card/rank card)) (inc (rank-value (:card/rank prev)))))
                (conj (pop groups) (conj (peek groups) card))
                (conj groups [card]))))
          []
          sorted-cards))

(defn runs
  "Every maximal run (3+ consecutive ranks, one suit) present in
   `hand` -- a seq of card-vectors, non-overlapping (a held 4-in-a-row
   is one length-4 run, not two overlapping length-3 runs)."
  [hand]
  (->> hand
       (group-by :card/suit)
       (mapcat (fn [[_suit cards]] (consecutive-groups (sort-by (comp rank-value :card/rank) cards))))
       (filter #(>= (count %) 3))))
