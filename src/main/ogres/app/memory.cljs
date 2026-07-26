(ns ogres.app.memory
  "Pure logic for the 'Memory' example game -- a concrete, playable
   demonstration of the generic prop-copy/shared-toggle mechanism
   (ogres.app.props, :object/shared?), not part of the generic engine
   itself. No DataScript, no UI; mirrors the role ogres.app.cards/
   ogres.app.props play for their own systems. See events.cljs's
   :memory/* methods and component/panel_memory.cljs.

   A card belongs to the current game iff its :object/variables map
   has a :memory/value key -- there's no separate 'session id' since
   this is a single-game-per-scene v1 (see the plan's scope cuts)."
  (:require [ogres.app.props :as props]
            [ogres.app.vec :refer [Vec2]]))

(defn value-image-hash
  "A deterministic data: URI SVG card face with a large centered number
   `n` -- distinct per value, generated on demand rather than seeded
   ahead of time (the same data: URI technique
   provider/state.cljs's bundled card-back/front demo images use)."
  [n]
  (str "data:image/svg+xml,"
       (js/encodeURIComponent
        (str "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 200 280'>"
             "<rect width='200' height='280' rx='14' fill='#f5f5f0'/>"
             "<rect x='6' y='6' width='188' height='268' rx='10' "
             "fill='none' stroke='#333333' stroke-width='3'/>"
             "<text x='100' y='172' font-size='108' font-family='sans-serif' "
             "text-anchor='middle' fill='#333333'>" n "</text>"
             "</svg>"))))

(defn deal
  "A shuffled deal of `n-values` matching pairs (2 * n-values cards
   total) onto a row-major grid `columns` wide, `spacing` scene-units
   apart -- a seq of {:memory/value v :point p} maps, one per card, in
   grid order (index 0 first). The pairing is shuffled across the WHOLE
   grid (not just within a row/column), so matching values land at
   unpredictable positions -- the actual 'deal'."
  [n-values columns spacing]
  (let [values (shuffle (into [] (mapcat (fn [v] [v v])) (range n-values)))]
    (map-indexed
     (fn [idx value]
       (let [[dx dy] (props/grid-offset idx columns spacing)]
         {:memory/value value :point (Vec2. dx dy)}))
     values)))

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
