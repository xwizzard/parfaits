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
            [ogres.app.component.card-pile :as card-pile]
            [ogres.app.hooks :as hooks]
            [uix.core :as uix :refer [defui $]]))

(def ^:private query
  [{:user/camera
    [{:camera/scene
      [{:scene/decks
        [:db/id :deck/name
         {:deck/cards [:db/id :card/rank :card/label :card/icon [:card/icon-color :default nil]
                       :card/location :card/position]}]}]}]}])

(defui ^:private deck [{:keys [entity dispatch]}]
  (let [{id :db/id name :deck/name cards :deck/cards} entity
        draw-count (count (filter (comp #{:draw} :card/location) cards))
        top (card-pile/top-of cards :discard)]
    ($ :li.decks-list-item
      ($ :.decks-list-name name)
      ($ :.decks-list-piles
        ($ :.decks-list-pile
          ($ card-pile/pile-card {:back? true :count draw-count}))
        ($ :.decks-list-pile
          ($ card-pile/pile-card {:card top})))
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
