(ns ogres.app.game-type.core-decks
  "The framework's built-in card-deck definitions -- pure data, no
   DataScript, no UI. A deck *definition* here is a reusable template;
   creating an *instance* (see events.cljs's :deck/create) copies a
   definition's cards into real per-scene card entities that then live
   and get shuffled/drawn/discarded independently of this template. Named
   `core-decks` (not nested under this namespace's own path) for the same
   reason `core-elements` is -- see its docstring.

   A future game module contributing its own deck (e.g. Gloomhaven's
   monster ability decks) plugs into the same registry purely by being
   added to the merge in `ogres.app.game-type` (see `deck-definitions`),
   without touching this map or the :deck/* event mechanism."
  (:require [clojure.string :refer [capitalize]]))

(def ^:private suit-icons
  {:clubs "suit-club-fill" :diamonds "suit-diamond-fill"
   :hearts "suit-heart-fill" :spades "suit-spade-fill"})

(def ^:private rank-labels
  {:jack "Jack" :queen "Queen" :king "King" :ace "Ace"})

(defn ^:private rank-label [rank]
  (get rank-labels rank (capitalize (name rank))))

(def definitions
  {:standard-52
   {:deck/name "Standard 52-Card Deck"
    :deck/cards
    (into []
          (for [suit [:clubs :diamonds :hearts :spades]
                rank [:two :three :four :five :six :seven :eight :nine :ten
                      :jack :queen :king :ace]]
            {:card/rank rank
             :card/suit suit
             :card/label (str (rank-label rank) " of " (capitalize (name suit)))
             :card/icon (suit-icons suit)}))
    ;; Optional, excluded unless :deck/create is called with
    ;; {:include-extras? true} -- demonstrates "special cards" as an
    ;; opt-in extra, not part of a deck's real count.
    :deck/extras
    [{:card/rank :joker :card/label "Joker" :card/icon "joker"}
     {:card/rank :joker :card/label "Joker" :card/icon "joker"}]}})
