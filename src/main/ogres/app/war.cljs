(ns ogres.app.war
  "Pure logic for the 'War' example game -- a demonstration of the
   generic card/deck system with no shared pile at all (every card
   belongs to exactly one player's personal draw/winnings piles) and
   no turn order (every active player plays simultaneously, every
   round -- ogres.app.turn-order is unused by this game entirely). No
   DataScript, no UI; mirrors the role every other example game's pure
   module plays. See events.cljs's :war/* methods and
   component/panel_war.cljs."
  (:require [ogres.app.cards :as cards]))

(defn tied-for-highest
  "Given `plays` ({player-id -> card}, one card per player for THIS
   comparison step), the set of player-ids tied for the highest
   :card/rank (ogres.app.cards/rank-value, ace-high -- the standard
   War convention) -- a singleton set means a clear winner; 2 or more
   means a war."
  [plays]
  (let [by-value (group-by (fn [[_ card]] (cards/rank-value (:card/rank card) {})) plays)
        max-value (apply max (keys by-value))]
    (into #{} (map first) (get by-value max-value))))
