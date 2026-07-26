(ns ogres.app.game-type.games.go-fish
  "Go Fish-specific game-type elements and its own deck definition.
   Demonstrates the same plugin pattern as games.dnd5e/games.gloomhaven
   for elements, plus the deck-registry extension point
   game_type/core_decks.cljs's own docstring already describes for a
   future module-owned deck (see `deck-definitions` below, merged into
   `ogres.app.game-type/deck-definitions` the exact same way
   `core-decks/definitions` already is).

   :go-fish/game gates the tab itself (mirrors :memory/game), kept
   deliberately separate from the four rule elements below it, so
   enabling a rule alone never reveals the tab. :go-fish/pair-scoring
   and :go-fish/book-scoring share an :exclusive-group -- the same
   mechanism dnd5e's and gloomhaven's HP trackers already use to pick
   one of several mutually exclusive alternatives -- since 'how many
   cards make a scored set' is a single either/or ruleset choice, not
   an independent on/off flag. The remaining three
   (extra-turn-on-hit/extra-turn-on-lucky-draw/ask-anyone) are
   independent booleans, checked directly from the active game-type's
   enabled-elements inside events.cljs's :go-fish/* methods -- no new
   configuration storage needed beyond the existing toggle system."
  (:require [clojure.string :refer [capitalize]]))

(def ^:private rank-labels
  {:two "Two" :three "Three" :four "Four" :five "Five" :six "Six"
   :seven "Seven" :eight "Eight" :nine "Nine" :ten "Ten"})

(defn ^:private rank-label [rank]
  (get rank-labels rank (capitalize (name rank))))

(def deck-definitions
  "Go Fish's own 36-card deck -- 9 unique ranks, 4 copies each, no
   suits (Go Fish matches by rank alone). Reuses the SAME :two..:ten
   rank vocabulary core-decks.cljs's Standard 52-Card Deck already
   uses (exactly 9 of its 13 ranks -- no jack/queen/king/ace), rather
   than inventing a parallel one, so ogres.app.cards/rank-value and any
   future rank-aware code already understands these cards too."
  {:go-fish-9
   {:deck/name "Go Fish (9 Ranks)"
    :deck/cards
    (into []
          (mapcat (fn [rank]
                    (repeat 4 {:card/rank rank
                               :card/label (rank-label rank)
                               :card/icon "card-front"})))
          [:two :three :four :five :six :seven :eight :nine :ten])}})

(def elements
  {:go-fish/game {:label "Go Fish" :icon "suit-heart-fill"}

   :go-fish/pair-scoring
   {:label "Pair Scoring (2 cards)"
    :icon "suit-heart-fill"
    :exclusive-group :go-fish-scoring-mode}

   :go-fish/book-scoring
   {:label "Book Scoring (4 cards)"
    :icon "suit-spade-fill"
    :exclusive-group :go-fish-scoring-mode}

   :go-fish/extra-turn-on-hit
   {:label "Extra Turn on Successful Ask"
    :icon "arrow-counterclockwise"}

   :go-fish/extra-turn-on-lucky-draw
   {:label "Extra Turn on Lucky Draw"
    :icon "dice-5-fill"}

   :go-fish/ask-anyone
   {:label "Ask Any Player"
    :icon "people-fill"}})
