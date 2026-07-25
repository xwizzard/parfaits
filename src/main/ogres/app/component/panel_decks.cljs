(ns ogres.app.component.panel-decks
  "The Decks panel/tab -- a minimal UI over the generic card/deck system
   (see ogres.app.cards, game-type/core-decks.cljs, and events.cljs's
   :deck/* methods). Gated by the :tool/cards element (see
   panel.cljs's visible-tabs), same as any other opt-in core primitive.

   Deliberately minimal: shows each deck's draw pile (a face-down card
   outline plus its count) and discard pile (a face-up card outline
   showing the top card's rank+suit -- a discard pile is always visible,
   same as a real one), plus Draw/Reset/Remove actions. Hands are fully
   reachable via events (:deck/deal, :deck/draw with a [:hand holder-id]
   target) but this pass has no hand-of-cards viewer yet -- that's a
   natural follow-up once a game module actually needs to show one."
  (:require [ogres.app.component :refer [icon]]
            [ogres.app.hooks :as hooks]
            [uix.core :as uix :refer [defui $]]))

(def ^:private query
  [{:user/camera
    [{:camera/scene
      [{:scene/decks
        [:db/id :deck/name
         {:deck/cards [:db/id :card/rank :card/label :card/icon :card/location :card/position]}]}]}]}])

(def ^:private rank-short
  "A compact corner abbreviation for a card-outline's rank, matching a
   real playing card's corner index -- nil (no abbreviation shown) for a
   rank without one (e.g. :joker, whose icon alone already identifies it)."
  {:two "2" :three "3" :four "4" :five "5" :six "6" :seven "7" :eight "8"
   :nine "9" :ten "10" :jack "J" :queen "Q" :king "K" :ace "A"})

(defn ^:private top-of [cards location]
  (let [matches (filter (comp #{location} :card/location) cards)]
    (if (seq matches) (apply max-key :card/position matches))))

(defui ^:private card-outline
  "A small card-shaped outline standing in for a pile -- a face-down back
   showing the draw pile's remaining count centered where a face-up
   card's suit icon would sit, a face-up card showing its rank+suit
   corner (the discard pile's top card), or an empty dashed placeholder
   (an empty pile). Deliberately basic -- a real per-card illustration is
   out of scope for this panel."
  [{:keys [card back? count]}]
  ($ :.decks-card
    {:data-back (boolean back?)
     :data-empty (and (not back?) (nil? card))}
    (cond
      back? ($ :.decks-card-count count)
      card ($ :<>
             (if-let [abbrev (rank-short (:card/rank card))]
               ($ :.decks-card-rank abbrev))
             ($ icon {:name (:card/icon card) :size 16})))))

(defui ^:private deck [{:keys [entity dispatch]}]
  (let [{id :db/id name :deck/name cards :deck/cards} entity
        draw-count (count (filter (comp #{:draw} :card/location) cards))
        top (top-of cards :discard)]
    ($ :li.decks-list-item
      ($ :.decks-list-name name)
      ($ :.decks-list-piles
        ($ :.decks-list-pile
          ($ card-outline {:back? true :count draw-count}))
        ($ :.decks-list-pile
          ($ card-outline {:card top})))
      ($ :.decks-list-buttons
        ($ :button.button.button-neutral
          {:type "button" :on-click #(dispatch :deck/draw id)}
          "Draw")
        ($ :button.button.button-neutral
          {:type "button" :disabled (empty? cards) :on-click #(dispatch :deck/reset id)}
          "Reset")
        ($ :button.button.button-danger
          {:type "button" :on-click #(dispatch :deck/remove id)}
          ($ icon {:name "trash3-fill" :size 16}))))))

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query)
        decks (:scene/decks (:camera/scene (:user/camera result)))]
    ($ :.form-decks
      ($ :header ($ :h2 "Decks"))
      (if (seq decks)
        ($ :ul.decks-list
          (for [entity decks]
            ($ deck {:key (:db/id entity) :entity entity :dispatch dispatch})))
        ($ :.form-notice
          "Add a deck below to draw and discard cards on this scene.")))))

(defui ^:memo actions []
  (let [dispatch (hooks/use-dispatch)]
    ($ :button.button.button-neutral
      {:type "button" :on-click #(dispatch :deck/create :standard-52)}
      ($ icon {:name "suit-spade-fill" :size 16})
      "Add Standard Deck")))
