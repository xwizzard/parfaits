(ns ogres.app.game-type.games.crazy-eights
  "Crazy 8s-specific game-type elements and its own deck definition.
   Demonstrates the same plugin pattern as games.go-fish/games.old-maid
   for elements, and reuses the *existing* Standard 52-Card Deck data
   (`core-decks/definitions`, already public) rather than inventing a
   new rank vocabulary -- same reuse call Old Maid made.

   The only change from the standard deck: the four 8s lose their
   :card/suit entirely (they're wild, and 'suit-less' is what makes
   that fall out of the normal matching rule for free, rather than
   needing a special case in ogres.app.crazy-eights/playable? for
   'well, unless it's an 8 -- then ignore its printed suit').

   :crazy-eights/game gates the tab (mirrors :memory/game/:go-fish/game/
   :old-maid/game). No rule-toggle elements -- unlike Go Fish, the
   ruleset here was fully specified by the user, so none are invented
   just to look symmetric."
  (:require [ogres.app.game-type.core-decks :as core-decks]))

(def deck-definitions
  "52 cards: the standard deck, except its four 8s are suit-less wilds
   (no :card/suit at all) instead of belonging to a suit like every
   other card. :card/icon-color is an optional per-card override
   component/card_hand.cljs and component/card_pile.cljs both check
   before falling back to their icon's default (inherited) color --
   the wild 8's star is the first card to ever use it."
  {:crazy-eights-52
   {:deck/name "Crazy 8s"
    :deck/cards
    (mapv (fn [card]
            (if (= (:card/rank card) :eight)
              (-> (dissoc card :card/suit)
                  (assoc :card/label "Crazy Eight" :card/icon "star"
                         :card/icon-color "var(--color-yellow-500)"))
              card))
          (:deck/cards (:standard-52 core-decks/definitions)))}})

(def elements
  {:crazy-eights/game {:label "Crazy 8s" :icon "magic"}})
