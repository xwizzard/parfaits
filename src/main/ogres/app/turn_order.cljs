(ns ogres.app.turn-order
  "Pure turn-cycle/scoring logic shared by any turn-based multiplayer
   example game (Memory, Go Fish, and whatever comes next) -- a
   wrapping player index that skips benched/removed seats without
   resizing or renumbering the seating order itself, and tied-highest-
   score winner resolution. No DataScript, no UI; mirrors the role
   ogres.app.cards/ogres.app.geom play for their own systems. Moved out
   of ogres.app.memory (where this first shipped) once Go Fish needed
   the identical logic -- nothing here was ever Memory-specific.")

(defn valid-turn-index
  "The first index at or after `idx` (wrapping through `players`, trying
   at most (count players) positions) whose player-id satisfies
   `active?` -- or nil if none do. Lets the turn cycle skip over
   benched/removed players without resizing or renumbering `players`
   itself; a re-activated player naturally rejoins at their original
   seat next time the cycle reaches them."
  [players active? idx]
  (let [n (count players)]
    (when (pos? n)
      (loop [i 0]
        (when (< i n)
          (let [candidate (mod (+ idx i) n)]
            (if (active? (nth players candidate))
              candidate
              (recur (inc i)))))))))

(defn next-turn-index
  "The turn index that follows `turn-index` in `players`' wrapping turn
   cycle, skipping forward past anyone `active?` now rejects -- nil if
   no one qualifies (e.g. every player benched at once)."
  [players active? turn-index]
  (valid-turn-index players active? (mod (inc turn-index) (count players))))

(defn winners
  "The set of player-ids tied for the highest score in `scores` (a
   {player-id count} map) -- empty if scores is empty. More than one
   id means a tie."
  [scores]
  (if (seq scores)
    (let [maximum (apply max (vals scores))]
      (into #{} (comp (filter (comp #{maximum} val)) (map key)) scores))
    #{}))
