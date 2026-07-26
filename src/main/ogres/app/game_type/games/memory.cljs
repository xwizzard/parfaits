(ns ogres.app.game-type.games.memory
  "Memory-specific game-type elements -- the card-matching example game
   built on ogres.app.memory/events.cljs's :memory/* methods.
   Demonstrates the same plugin pattern as games.dnd5e/games.gloomhaven:
   this namespace's `elements` map merges into the shared registry (see
   `ogres.app.game-type`) purely by being required there -- nothing in
   the core rendering layer (panel.cljs) ever names this namespace
   directly, it only checks whether :memory/game happens to be enabled
   for the active game-type (see panel.cljs's visible-tabs).")

(def elements
  {:memory/game {:label "Memory" :icon "card-front"}})
