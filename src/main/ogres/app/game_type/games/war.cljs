(ns ogres.app.game-type.games.war
  "War-specific game-type elements -- no deck definition at all, same
   reuse tier as Rummy: the standard 52-card deck, completely
   unmodified.

   :war/game gates the tab (mirrors every other example game's own
   :*/game element). No rule-toggle elements -- the ruleset here has
   no real variants described, so none are invented just to look
   symmetric with Go Fish/Rummy.")

(def elements
  {:war/game {:label "War" :icon "fist"}})
