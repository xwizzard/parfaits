(ns ogres.app.dice
  "Pure dice-rolling logic -- a generic n-sided-die primitive, not tied
   to any particular game-type. No DataScript, no UI; mirrors the role
   ogres.app.cards/ogres.app.war play for their own systems. See
   events.cljs's :dice/* methods and component/panel_dice.cljs.

   `best`/`worst`/`sum` all take ALREADY-ROLLED data ({:sides :value}
   maps), not raw side-counts -- this is what makes them deterministically
   testable with hand-built fixtures, no RNG involved, and it's also
   what lets one shape serve both a plain multi-die pool (summed) and
   D&D 5e's advantage/disadvantage (best/worst single value among
   however many dice were rolled) -- those aren't two features, just
   two different ways of combining the same rolled-dice data.")

(defn roll-die
  "One roll of a single `sides`-sided die, 1 through `sides` inclusive."
  [sides]
  (inc (rand-int sides)))

(defn roll-dice
  "Rolls each of `sides-seq` (e.g. [20 6 6] for 1d20 + 2d6) once each, in
   order, returning [{:sides n :value v} ...] -- the raw, ungrouped
   result of one roll action."
  [sides-seq]
  (mapv (fn [sides] {:sides sides :value (roll-die sides)}) sides-seq))

(defn sum
  "The total of every rolled die's value -- the default way a plain
   multi-die pool (no advantage/disadvantage) combines into one number."
  [rolled]
  (apply + (map :value rolled)))

(defn best
  "The single rolled die with the highest value -- advantage's combining
   rule: roll extra dice, take the best one, discard the rest."
  [rolled]
  (apply max-key :value rolled))

(defn worst
  "The single rolled die with the lowest value -- disadvantage's
   combining rule: roll extra dice, take the worst one, discard the
   rest."
  [rolled]
  (apply min-key :value rolled))
