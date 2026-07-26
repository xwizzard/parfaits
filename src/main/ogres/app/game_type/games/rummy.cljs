(ns ogres.app.game-type.games.rummy
  "Rummy-specific game-type elements -- no deck definition at all,
   unlike Old Maid/Crazy 8s (both derived a modified deck from the
   existing Standard 52-Card Deck); Rummy needs zero changes to it,
   the purest reuse case yet.

   :rummy/game gates the tab (mirrors :memory/game/:go-fish/game/
   :old-maid/game/:crazy-eights/game), kept separate from the rule
   element below it so enabling the rule alone never reveals the tab
   -- same reasoning games.go-fish.cljs's own docstring gives.
   :rummy/runs is an independent boolean (off by default), checked
   directly from the active game-type's enabled-elements inside
   events.cljs's :rummy/* methods -- Go Fish's exact pattern for its
   own off-by-default rule toggles (extra-turn-on-hit etc). Unlike Go
   Fish's mutually-exclusive scoring-mode pair, run-melds are a real
   ADDITION to sets, not an alternative ruleset, so no
   :exclusive-group here.")

(def elements
  {:rummy/game {:label "Rummy" :icon "suit-diamond-fill"}

   :rummy/runs
   {:label "Allow Runs (3+ Consecutive, One Suit)"
    :icon "arrow-right-short"}})
