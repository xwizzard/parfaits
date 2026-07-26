(ns ogres.app.game-type.games.old-maid
  "Old Maid-specific game-type elements and its own deck definition.
   Demonstrates the same plugin pattern as games.dnd5e/games.gloomhaven/
   games.go-fish for elements, and reuses the *existing* Standard
   52-Card Deck data (`core-decks/definitions`, already public) rather
   than inventing a new rank vocabulary -- the traditional way Old Maid
   is actually played: drop 3 of the 4 queens and re-skin the one that
   survives as the unmatched 'Old Maid' card, no new ranks needed at
   all.

   :old-maid/game gates the tab (mirrors :memory/game/:go-fish/game).
   No rule-toggle elements -- unlike Go Fish, nothing about this
   ruleset is meaningfully configurable, so none are invented just to
   look symmetric."
  (:require [ogres.app.game-type.core-decks :as core-decks]))

(def deck-definitions
  "49 cards: 48 non-queens (12 ranks x 4 suits, forming 24 pairs by
   rank alone -- suit is irrelevant to matching, same as real Old
   Maid) plus 1 reskinned queen that can never pair, her 3 sisters
   having been removed."
  {:old-maid-52
   {:deck/name "Old Maid"
    :deck/cards
    (let [all (:deck/cards (:standard-52 core-decks/definitions))
          [keep] (filter (comp #{:queen} :card/rank) all)
          others (remove (comp #{:queen} :card/rank) all)]
      (conj (vec others) (assoc keep :card/label "Old Maid" :card/icon "skull")))}})

(def elements
  {:old-maid/game {:label "Old Maid" :icon "skull"}})
