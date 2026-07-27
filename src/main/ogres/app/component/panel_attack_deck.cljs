(ns ogres.app.component.panel-attack-deck
  "The 'Attack Modifier Decks' panel/tab -- a minimal UI over events.cljs's
   :attack-deck/* methods and ogres.app.attack-deck's pure kind-vocabulary/
   comparison logic. Gated by :gloomhaven/attack-deck (see panel.cljs's
   visible-tabs), visible to host and guest alike in both Setup and Play
   -- unlike the Dice tab, creating decks and applying perk/item edits is
   naturally a setup-time activity too, not play-only.

   One row per deck (a roster player's own personal deck, or the shared
   'Monsters' deck), each with a draw-pile/discard-pile count, a Normal/
   Advantage/Disadvantage mode picker plus Draw button, and a collapsible
   composition editor (add/remove copies of a kind, or add a BLESS/
   CURSE) -- the generic perk/item deck-edit vocabulary, not official
   class content (see ogres.app.attack-deck's own docstring). A shared
   draw history log mirrors the dice feature's own :scene/dice-rolls
   list. A host-only 'Reshuffle Flagged Decks' action in the footer
   sweeps every deck whose last draw was Null/2x -- see :attack-deck/
   reshuffle-flagged's own docstring for why this is a manual action
   rather than auto-wired into the generic initiative/round system."
  (:require [ogres.app.component :refer [icon]]
            [ogres.app.hooks :as hooks]
            [ogres.app.player :as player]
            [uix.core :as uix :refer [defui $]]))

(def ^:private kind-label
  {:minus-2 "-2" :minus-1 "-1" :plus-0 "+0" :plus-1 "+1" :plus-2 "+2"
   :null "Null" :times-2 "2x" :bless "BLESS" :curse "CURSE"})

(def ^:private standard-kinds
  "The 7 base kinds offered by the composition editor's add/remove rows
   -- BLESS/CURSE are added one at a time via their own dedicated
   buttons instead (they're one-shot, not a count you'd dial up)."
  [:minus-2 :minus-1 :plus-0 :plus-1 :plus-2 :null :times-2])

(def ^:private query
  [{:root/user
    [:user/uuid
     :user/host
     {:user/camera
      [{:camera/scene
        [{:scene/attack-decks
          [:db/id :deck/name
           [:deck/needs-reshuffle? :default false]
           {:deck/owner [:db/id :player/name :player/color {:player/controller [:user/uuid]}]}
           {:deck/cards [:card/rank :card/location]}]}
         {:scene/attack-draws
          [:db/id
           [:draw/mode :default nil]
           :draw/kind
           [:draw/discarded-kind :default nil]
           [:draw/at :default 0]
           {:draw/deck [:db/id :deck/name]}]}]}]}]}
   {:root/players [:db/id :player/name :player/color]}
   {:root/session [{:session/conns [:user/uuid]}]}])

(defn ^:private attack-decks-state
  [result]
  (let [{uuid :user/uuid host :user/host
         {scene :camera/scene} :user/camera} (:root/user result)
        decks (:scene/attack-decks scene)
        connected (into #{} (map :user/uuid) (:session/conns (:root/session result)))
        authorized?
        (fn [owner]
          (if owner
            (player/authority? uuid host connected (get-in owner [:player/controller :user/uuid]))
            host))
        owned-ids (into #{} (keep (comp :db/id :deck/owner)) decks)]
    {:host host
     :decks decks
     :draws (sort-by :draw/at > (:scene/attack-draws scene))
     :authorized? authorized?
     :has-monster? (some (comp nil? :deck/owner) decks)
     :players-without-deck (remove (comp owned-ids :db/id) (:root/players result))}))

(defui ^:private composition-form
  [{:keys [dispatch deck-id disabled?]}]
  (let [[deltas set-deltas] (uix/use-state {})]
    ($ :form.attack-deck-composition-form
      {:on-submit
       (fn [event]
         (.preventDefault event)
         (doseq [[kind n] deltas]
           (cond
             (pos? n) (dispatch :attack-deck/add-cards deck-id kind n)
             (neg? n) (dispatch :attack-deck/remove-cards deck-id kind (- n))))
         (set-deltas {}))}
      ($ :fieldset.fieldset.attack-deck-composition-rows
        {:disabled disabled?}
        ($ :legend "Adjust Composition")
        (for [kind standard-kinds]
          ($ :.attack-deck-composition-row {:key kind}
            ($ :label.attack-deck-composition-label (kind-label kind))
            ($ :input.text.attack-deck-composition-delta
              {:type "number" :step 1
               :value (get deltas kind 0)
               :on-change
               (fn [event]
                 (let [n (js/Number (.. event -target -value))]
                   (set-deltas (fn [d] (if (zero? n) (dissoc d kind) (assoc d kind n))))))}))))
      ($ :button.button.button-neutral
        {:type "submit" :disabled (or disabled? (empty? deltas))}
        "Apply")
      ($ :.attack-deck-composition-special
        ($ :button.button.button-neutral
          {:type "button" :disabled disabled? :on-click #(dispatch :attack-deck/add-bless deck-id 1)}
          "Add BLESS")
        ($ :button.button.button-neutral
          {:type "button" :disabled disabled? :on-click #(dispatch :attack-deck/add-curse deck-id 1)}
          "Add CURSE")))))

(defui ^:private draw-form
  [{:keys [dispatch deck-id disabled?]}]
  (let [[mode set-mode] (uix/use-state nil)]
    ($ :form.attack-deck-draw-form
      {:on-submit (fn [event] (.preventDefault event) (dispatch :attack-deck/draw deck-id mode))}
      ($ :fieldset.fieldset.attack-deck-mode-row
        {:disabled disabled?}
        ($ :legend "Mode")
        (for [[value label] [[nil "Normal"] [:advantage "Advantage"] [:disadvantage "Disadvantage"]]]
          ($ :label.radio {:key (str value)}
            ($ :input
              {:type "radio"
               :name (str "attack-deck-mode-" deck-id)
               :checked (= mode value)
               :on-change (fn [] (set-mode value))})
            label)))
      ($ :button.button.button-neutral
        {:type "submit" :disabled disabled?}
        ($ icon {:name "suit-spade-fill" :size 16})
        "Draw"))))

(defui ^:private deck-row [{:keys [deck dispatch authorized?]}]
  (let [[editing? set-editing] (uix/use-state false)
        {id :db/id name :deck/name owner :deck/owner cards :deck/cards
         flagged :deck/needs-reshuffle?} deck
        draw-count (count (filter (comp #{:draw} :card/location) cards))
        discard-count (count (filter (comp #{:discard} :card/location) cards))]
    ($ :li.attack-deck-row
      ($ :.attack-deck-row-header
        ($ :span.attack-deck-row-name {:data-color (:player/color owner)} name)
        (if flagged ($ :span.attack-deck-row-flag "Needs Reshuffle"))
        ($ :span.attack-deck-row-counts (str draw-count " draw / " discard-count " discard")))
      ($ draw-form {:dispatch dispatch :deck-id id :disabled? (not authorized?)})
      ($ :button.button.button-neutral
        {:type "button" :on-click #(set-editing not)}
        (if editing? "Hide Composition" "Edit Composition"))
      (if editing?
        ($ composition-form {:dispatch dispatch :deck-id id :disabled? (not authorized?)})))))

(defui ^:private draw-item [{:keys [draw]}]
  (let [{deck :draw/deck mode :draw/mode kind :draw/kind other :draw/discarded-kind} draw]
    ($ :li.attack-deck-draw-item
      ($ :.attack-deck-draw-header
        ($ :span.attack-deck-draw-owner (:deck/name deck))
        (if mode ($ :span.attack-deck-draw-mode (name mode)))
        ($ :span.attack-deck-draw-kind (kind-label kind)))
      (if other
        ($ :.attack-deck-draw-discarded (str "discarded: " (kind-label other)))))))

(defui ^:private new-deck-controls
  [{:keys [dispatch players-without-deck has-monster? host?]}]
  (if host?
    ($ :.attack-deck-new-controls
      (if-not has-monster?
        ($ :button.button.button-neutral
          {:type "button" :on-click #(dispatch :attack-deck/create nil)}
          "Add Monster Deck"))
      (for [p players-without-deck]
        ($ :button.button.button-neutral
          {:key (:db/id p) :type "button"
           :on-click #(dispatch :attack-deck/create (:db/id p))}
          (str "Add Deck: " (:player/name p)))))))

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        {:keys [host decks draws authorized? has-monster? players-without-deck]}
        (attack-decks-state result)]
    ($ :.form-attack-deck
      ($ :header ($ :h2 "Attack Modifier Decks"))
      (if (seq decks)
        ($ :ul.attack-deck-list
          (for [deck decks]
            ($ deck-row
              {:key (:db/id deck) :deck deck :dispatch dispatch
               :authorized? (authorized? (:deck/owner deck))})))
        ($ :.form-notice "No decks yet -- create one below."))
      ($ new-deck-controls
        {:dispatch dispatch :players-without-deck players-without-deck
         :has-monster? has-monster? :host? host})
      (if (seq draws)
        ($ :ul.attack-deck-draw-list
          (for [draw draws]
            ($ draw-item {:key (:db/id draw) :draw draw})))))))

(defui ^:memo actions []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        {:keys [host decks]} (attack-decks-state result)
        flagged? (boolean (some :deck/needs-reshuffle? decks))]
    (if host
      ($ :button.button.button-danger
        {:type "button" :disabled (not flagged?) :on-click #(dispatch :attack-deck/reshuffle-flagged)}
        ($ icon {:name "arrow-counterclockwise" :size 16})
        "Reshuffle Flagged Decks"))))
