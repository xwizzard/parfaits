(ns ogres.app.component.card-pile
  "Shared 'pile of cards' UI -- a face-down back showing a remaining
   count, a face-up card showing its rank+suit corner (a discard pile's
   live top card), or an empty dashed placeholder -- extracted once a
   second consumer (Crazy 8s' draw/discard piles, component/panel_
   crazy_eights.cljs) needed the identical piece panel_decks.cljs's
   Decks tab first built for its own draw/discard display. Also now the
   single home for rank-short, dropping the identical copy panel_decks.
   cljs and component/card_hand.cljs each previously kept separately."
  (:require [ogres.app.component :refer [icon]]
            [uix.core :refer [defui $]]))

(def rank-short
  "A compact corner abbreviation for a card's rank, matching a real
   playing card's corner index -- nil (no abbreviation shown) for a
   rank without one (e.g. :joker, whose icon alone already identifies
   it)."
  {:two "2" :three "3" :four "4" :five "5" :six "6" :seven "7" :eight "8"
   :nine "9" :ten "10" :jack "J" :queen "Q" :king "K" :ace "A"})

(defn top-of
  "The card in `cards` at `location` (e.g. :discard) with the highest
   :card/position -- nil if none. 'Top of the pile' by the same
   position-as-order-key idiom events.cljs's own top-card uses."
  [cards location]
  (let [matches (filter (comp #{location} :card/location) cards)]
    (if (seq matches) (apply max-key :card/position matches))))

(defui pile-card
  "A small card-shaped outline standing in for a pile -- a face-down
   back showing the draw pile's remaining count centered where a
   face-up card's suit icon would sit, a face-up card showing its
   rank+suit corner (the discard pile's top card), or an empty dashed
   placeholder (an empty pile). Deliberately basic -- a real per-card
   illustration is out of scope."
  [{:keys [card back? count]}]
  ($ :.card-pile
    {:data-back (boolean back?)
     :data-empty (and (not back?) (nil? card))}
    (cond
      back? ($ :.card-pile-count count)
      card ($ :<>
             (if-let [abbrev (rank-short (:card/rank card))]
               ($ :.card-pile-rank abbrev))
             ($ icon {:name (:card/icon card) :size 16 :color (:card/icon-color card)})))))
