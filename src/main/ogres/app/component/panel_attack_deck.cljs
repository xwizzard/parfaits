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
  (:require [ogres.app.attack-deck :as attack-deck]
            [ogres.app.component :refer [icon]]
            [ogres.app.hooks :as hooks]
            [ogres.app.player :as player]
            [uix.core :as uix :refer [defui $]]))

(def ^:private kind-label
  {:minus-2 "-2" :minus-1 "-1" :plus-0 "+0" :plus-1 "+1" :plus-2 "+2"
   :null "Null" :times-2 "2x" :bless "BLESS" :curse "CURSE"})

(def ^:private standard-kinds
  "The 7 base kinds offered by the composition editor's add/remove rows
   and the special-effect-card form's own base-kind select -- BLESS/
   CURSE are added one at a time via their own dedicated buttons instead
   (they're one-shot, not a count you'd dial up), and never carry an
   attached effect (see ogres.app.attack-deck/effect-kinds)."
  [:minus-2 :minus-1 :plus-0 :plus-1 :plus-2 :null :times-2])

(def ^:private reduced-randomness-label
  "Reduced Randomness variant (p.49) display overrides -- :times-2/
   :bless read as a flat '+2', :null/:curse as a flat '-2', when the
   variant is on. Purely cosmetic (see :attack-deck/toggle-reduced-
   randomness's own docstring) -- comparison/shuffle-triggering logic
   never consults this."
  {:times-2 "+2" :bless "+2" :null "-2" :curse "-2"})

(defn ^:private draw-text
  "Display text for one drawn/held kind + its optional attached effect,
   e.g. \"+1\" or \"+1 Push 2\" -- \"-2\" instead of \"Null\"/\"CURSE\"
   or \"+2\" instead of \"2x\"/\"BLESS\" when `reduced?` (the Reduced
   Randomness variant) is on."
  [kind effect amount reduced?]
  (let [label (if reduced? (get reduced-randomness-label kind (kind-label kind)) (kind-label kind))
        effect-text (attack-deck/effect-label effect amount)]
    (if effect-text (str label " " effect-text) label)))

(defn ^:private effect-card-groups
  "The deck's current effect cards, grouped by their exact (kind, effect,
   amount) triple with a count -- what the composition editor's removal
   list shows, one row per distinct combination rather than one per
   physical card."
  [cards]
  (->> cards
       (filter :card/effect)
       (group-by (juxt :card/rank :card/effect :card/effect-amount))
       (map (fn [[[kind effect amount] group]]
              {:kind kind :effect effect :amount amount :count (count group)}))
       (sort-by (juxt :kind :effect))))

(defn ^:private temporary-card-groups
  "The deck's current :card/temporary? cards (BLESS/CURSE plus any item/
   scenario-added plain or effect card), grouped the same way effect-
   card-groups is -- what an :attack-deck/end-scenario sweep would
   currently clear from this deck. Overlaps with effect-card-groups
   whenever a card is both temporary AND carries an effect; shown as two
   separate lenses rather than one combined view."
  [cards]
  (->> cards
       (filter :card/temporary?)
       (group-by (juxt :card/rank :card/effect :card/effect-amount))
       (map (fn [[[kind effect amount] group]]
              {:kind kind :effect effect :amount amount :count (count group)}))
       (sort-by (juxt :kind :effect))))

(def ^:private deck-card-fields
  [:card/rank :card/location
   [:card/effect :default nil] [:card/effect-amount :default nil]
   [:card/temporary? :default nil]])

(def ^:private query
  [{:root/user
    [:user/uuid
     :user/host
     {:user/camera
      [{:camera/scene
        [[:scene/attack-deck-shuffle-icons? :default true]
         [:scene/attack-deck-reduced-randomness? :default false]
         {:scene/monster-attack-deck
          [:db/id :deck/name [:deck/needs-reshuffle? :default false] {:deck/cards deck-card-fields}]}
         {:scene/attack-draws
          [:db/id
           [:draw/mode :default nil]
           :draw/kind
           [:draw/effect :default nil]
           [:draw/effect-amount :default nil]
           [:draw/discarded-kind :default nil]
           [:draw/discarded-effect :default nil]
           [:draw/discarded-effect-amount :default nil]
           [:draw/at :default 0]
           {:draw/deck [:db/id :deck/name]}]}]}]}]}
   {:root/players
    [:db/id :player/name :player/color {:player/controller [:user/uuid]}
     {:player/attack-deck
      [:db/id :deck/name [:deck/needs-reshuffle? :default false] {:deck/cards deck-card-fields}]}]}
   {:root/session [{:session/conns [:user/uuid]}]}])

(defn ^:private attack-decks-state
  [result]
  (let [{uuid :user/uuid host :user/host
         {scene :camera/scene} :user/camera} (:root/user result)
        players (:root/players result)
        monster-deck (:scene/monster-attack-deck scene)
        draws (:scene/attack-draws scene)
        connected (into #{} (map :user/uuid) (:session/conns (:root/session result)))
        authorized?
        (fn [owner]
          (if owner
            (player/authority? uuid host connected (get-in owner [:player/controller :user/uuid]))
            host))
        ;; Each player's own :player/attack-deck is a forward ref -- the
        ;; deck itself no longer stores who owns it (see events.cljs's
        ;; :player/_attack-deck reverse-ref note), so :deck/owner is
        ;; reconstructed here, client-side, purely so deck-row/draw-item
        ;; below need no changes at all -- they already just expect a
        ;; plain deck map with a :deck/owner key, however it got there.
        player-decks (keep (fn [p]
                              (some-> (:player/attack-deck p)
                                      (assoc :deck/owner (select-keys p [:db/id :player/name :player/color :player/controller]))))
                            players)
        decks (cond-> (vec player-decks) monster-deck (conj monster-deck))]
    {:host host
     :decks decks
     :draws (sort-by :draw/at > draws)
     :draws-by-deck
     (into {} (map (fn [[deck-id ds]] [deck-id (apply max-key :draw/at ds)]))
           (group-by (comp :db/id :draw/deck) draws))
     :authorized? authorized?
     :shuffle-icons? (:scene/attack-deck-shuffle-icons? scene)
     :reduced-randomness? (:scene/attack-deck-reduced-randomness? scene)
     :has-monster? (some? monster-deck)
     :players-without-deck (remove :player/attack-deck players)}))

(defui ^:private composition-form
  [{:keys [dispatch deck-id disabled?]}]
  (let [[deltas set-deltas] (uix/use-state {})
        [temporary? set-temporary] (uix/use-state false)]
    ($ :form.attack-deck-composition-form
      {:on-submit
       (fn [event]
         (.preventDefault event)
         (doseq [[kind n] deltas]
           (cond
             (pos? n) (dispatch :attack-deck/add-cards deck-id kind n temporary?)
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
      ($ :label.checkbox.attack-deck-temporary-toggle
        ($ :input
          {:type "checkbox" :disabled disabled? :checked temporary?
           :on-change (fn [event] (set-temporary (.. event -target -checked)))})
        "Mark additions as temporary (item/scenario, removed at end of scenario)")
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

(defui ^:private replace-card-form
  "Replaces one PLAIN card of `from-kind` with one of `to-kind` -- the
   generic perk/item deck-edit primitive for 'replace one -2 card with
   one -1 card' (see events.cljs's :attack-deck/replace-card)."
  [{:keys [dispatch deck-id disabled?]}]
  (let [[from-kind set-from] (uix/use-state :minus-2)
        [to-kind set-to] (uix/use-state :minus-1)]
    ($ :form.attack-deck-replace-form
      {:on-submit (fn [event] (.preventDefault event) (dispatch :attack-deck/replace-card deck-id from-kind to-kind))}
      ($ :legend "Replace Card")
      ($ :select.attack-deck-replace-select
        {:value (name from-kind) :disabled disabled?
         :on-change (fn [event] (set-from (keyword (.. event -target -value))))}
        (for [k standard-kinds] ($ :option {:key k :value (name k)} (kind-label k))))
      ($ :span.attack-deck-replace-arrow "→")
      ($ :select.attack-deck-replace-select
        {:value (name to-kind) :disabled disabled?
         :on-change (fn [event] (set-to (keyword (.. event -target -value))))}
        (for [k standard-kinds] ($ :option {:key k :value (name k)} (kind-label k))))
      ($ :button.button.button-neutral {:type "submit" :disabled disabled?} "Replace"))))

(defui ^:private effect-card-list
  "The deck's current special-effect cards (grouped, see effect-card-
   groups), each with a Remove button dispatching :attack-deck/remove-
   effect-cards for that exact (kind, effect, amount) triple."
  [{:keys [dispatch deck-id cards disabled? reduced?]}]
  (let [groups (effect-card-groups cards)]
    (if (seq groups)
      ($ :ul.attack-deck-effect-list
        (for [{:keys [kind effect amount count]} groups]
          ($ :li.attack-deck-effect-item {:key (str kind "-" effect "-" amount)}
            ($ :span.attack-deck-effect-item-label (str (draw-text kind effect amount reduced?) " x" count))
            ($ :button.button.button-danger
              {:type "button" :disabled disabled?
               :on-click #(dispatch :attack-deck/remove-effect-cards deck-id kind effect amount 1)}
              ($ icon {:name "trash3-fill" :size 14}))))))))

(defui ^:private temporary-card-list
  "The deck's current :card/temporary? cards (see temporary-card-groups)
   -- what an :attack-deck/end-scenario sweep would clear from THIS
   deck. Removal here is per-group (same shape as effect-card-list), via
   whichever of :attack-deck/remove-cards/remove-effect-cards actually
   matches (plain vs effect-bearing)."
  [{:keys [dispatch deck-id cards disabled? reduced?]}]
  (let [groups (temporary-card-groups cards)]
    (if (seq groups)
      ($ :<>
        ($ :h3.attack-deck-temporary-heading "Temporary Cards")
        ($ :ul.attack-deck-effect-list
          (for [{:keys [kind effect amount count]} groups]
            ($ :li.attack-deck-effect-item {:key (str "temp-" kind "-" effect "-" amount)}
              ($ :span.attack-deck-effect-item-label (str (draw-text kind effect amount reduced?) " x" count))
              ($ :button.button.button-danger
                {:type "button" :disabled disabled?
                 :on-click
                 (fn []
                   (if effect
                     (dispatch :attack-deck/remove-effect-cards deck-id kind effect amount 1)
                     (dispatch :attack-deck/remove-cards deck-id kind 1)))}
                ($ icon {:name "trash3-fill" :size 14})))))))))

(defui ^:private effect-card-form
  "Adds a special-effect card (a base kind plus an attached effect, e.g.
   '+1 Push 2') -- the generic perk/item deck-edit primitive for the
   majority of real class perks, which add more than a plain ±N (see
   ogres.app.attack-deck/effect-kinds and events.cljs's :attack-deck/
   add-effect-cards)."
  [{:keys [dispatch deck-id disabled?]}]
  (let [[kind set-kind] (uix/use-state :plus-0)
        [effect set-effect] (uix/use-state :push)
        [amount set-amount] (uix/use-state 1)
        [count-n set-count] (uix/use-state 1)
        [temporary? set-temporary] (uix/use-state false)
        amount? (:amount? (get attack-deck/effect-kinds effect))]
    ($ :form.attack-deck-effect-form
      {:on-submit
       (fn [event]
         (.preventDefault event)
         (dispatch :attack-deck/add-effect-cards deck-id kind effect (if amount? amount) count-n temporary?))}
      ($ :select.attack-deck-effect-kind-select
        {:value (name kind) :disabled disabled?
         :on-change (fn [event] (set-kind (keyword (.. event -target -value))))}
        (for [k standard-kinds]
          ($ :option {:key k :value (name k)} (kind-label k))))
      ($ :select.attack-deck-effect-select
        {:value (name effect) :disabled disabled?
         :on-change (fn [event] (set-effect (keyword (.. event -target -value))))}
        (for [[k {:keys [label]}] attack-deck/effect-kinds]
          ($ :option {:key k :value (name k)} label)))
      (if amount?
        ($ :input.text.attack-deck-effect-amount
          {:type "number" :min 1 :disabled disabled?
           :value amount :on-change (fn [event] (set-amount (js/Number (.. event -target -value))))}))
      ($ :input.text.attack-deck-effect-count
        {:type "number" :min 1 :disabled disabled?
         :value count-n :on-change (fn [event] (set-count (js/Number (.. event -target -value))))})
      ($ :label.checkbox.attack-deck-temporary-toggle
        ($ :input
          {:type "checkbox" :disabled disabled? :checked temporary?
           :on-change (fn [event] (set-temporary (.. event -target -checked)))})
        "Temporary")
      ($ :button.button.button-neutral {:type "submit" :disabled disabled?} "Add Special Card"))))

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

(defui ^:private deck-row [{:keys [deck dispatch authorized? latest-draw reduced?]}]
  (let [[editing? set-editing] (uix/use-state false)
        ;; NOT `name` -- shadowing clojure.core/name here previously
        ;; broke the (name (:draw/mode ...)) call below the instant a
        ;; deck's last draw was an Advantage/Disadvantage, since the
        ;; deck's own name STRING got called as if it were a function.
        {id :db/id deck-name :deck/name owner :deck/owner cards :deck/cards
         flagged :deck/needs-reshuffle?} deck
        draw-count (count (filter (comp #{:draw} :card/location) cards))
        discard-count (count (filter (comp #{:discard} :card/location) cards))]
    ($ :li.attack-deck-row
      ($ :.attack-deck-row-header
        ($ :span.attack-deck-row-name {:data-color (:player/color owner)} deck-name)
        (if flagged ($ :span.attack-deck-row-flag "Needs Reshuffle"))
        ($ :span.attack-deck-row-counts (str draw-count " draw / " discard-count " discard")))
      (if latest-draw
        ($ :.attack-deck-row-last-drawn
          "Last drawn: "
          (if (:draw/mode latest-draw) (str (name (:draw/mode latest-draw)) " -- "))
          (draw-text (:draw/kind latest-draw) (:draw/effect latest-draw) (:draw/effect-amount latest-draw) reduced?)))
      ($ draw-form {:dispatch dispatch :deck-id id :disabled? (not authorized?)})
      ($ :.attack-deck-row-buttons
        ($ :button.button.button-neutral
          {:type "button" :on-click #(set-editing not)}
          (if editing? "Hide Composition" "Edit Composition"))
        ($ :button.button.button-danger
          {:type "button" :disabled (not authorized?) :on-click #(dispatch :attack-deck/reset id)}
          "Reset Deck")
        ($ :button.button.button-danger
          {:type "button" :disabled (not authorized?) :on-click #(dispatch :attack-deck/remove id)}
          ($ icon {:name "trash3-fill" :size 14})
          "Remove Deck"))
      (if editing?
        ($ :<>
          ($ composition-form {:dispatch dispatch :deck-id id :disabled? (not authorized?)})
          ($ replace-card-form {:dispatch dispatch :deck-id id :disabled? (not authorized?)})
          ($ effect-card-list {:dispatch dispatch :deck-id id :cards cards :disabled? (not authorized?) :reduced? reduced?})
          ($ effect-card-form {:dispatch dispatch :deck-id id :disabled? (not authorized?)})
          ($ temporary-card-list {:dispatch dispatch :deck-id id :cards cards :disabled? (not authorized?) :reduced? reduced?}))))))

(defui ^:private draw-item [{:keys [draw reduced?]}]
  (let [{deck :draw/deck mode :draw/mode kind :draw/kind other :draw/discarded-kind
         effect :draw/effect amount :draw/effect-amount
         other-effect :draw/discarded-effect other-amount :draw/discarded-effect-amount} draw]
    ($ :li.attack-deck-draw-item
      ($ :.attack-deck-draw-header
        ($ :span.attack-deck-draw-owner (:deck/name deck))
        (if mode ($ :span.attack-deck-draw-mode (name mode)))
        ($ :span.attack-deck-draw-kind (draw-text kind effect amount reduced?)))
      (if other
        ($ :.attack-deck-draw-discarded (str "discarded: " (draw-text other other-effect other-amount reduced?)))))))

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
        {:keys [host decks draws draws-by-deck authorized? shuffle-icons? reduced-randomness?
                has-monster? players-without-deck]}
        (attack-decks-state result)]
    ($ :.form-attack-deck
      ($ :header
        ($ :h2 "Attack Modifier Decks")
        (if host
          ($ :<>
            ($ :label.checkbox.attack-deck-shuffle-icons-toggle
              ($ :input
                {:type "checkbox"
                 :checked shuffle-icons?
                 :on-change (fn [event] (dispatch :attack-deck/toggle-shuffle-icons (.. event -target -checked)))})
              "Reshuffle on Null/2x")
            ($ :label.checkbox.attack-deck-shuffle-icons-toggle
              ($ :input
                {:type "checkbox"
                 :checked reduced-randomness?
                 :on-change (fn [event] (dispatch :attack-deck/toggle-reduced-randomness (.. event -target -checked)))})
              "Reduced Randomness"))))
      (if (seq decks)
        ($ :ul.attack-deck-list
          (for [deck decks]
            ($ deck-row
              {:key (:db/id deck) :deck deck :dispatch dispatch
               :authorized? (authorized? (:deck/owner deck))
               :latest-draw (get draws-by-deck (:db/id deck))
               :reduced? reduced-randomness?})))
        ($ :.form-notice "No decks yet -- create one below."))
      ($ new-deck-controls
        {:dispatch dispatch :players-without-deck players-without-deck
         :has-monster? has-monster? :host? host})
      (if (seq draws)
        ($ :ul.attack-deck-draw-list
          (for [draw draws]
            ($ draw-item {:key (:db/id draw) :draw draw :reduced? reduced-randomness?})))))))

(defui ^:memo actions []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        {:keys [host decks]} (attack-decks-state result)
        flagged? (boolean (some :deck/needs-reshuffle? decks))
        has-temporary? (boolean (some (fn [d] (some :card/temporary? (:deck/cards d))) decks))]
    (if host
      ($ :<>
        ($ :button.button.button-danger
          {:type "button" :disabled (not flagged?) :on-click #(dispatch :attack-deck/reshuffle-flagged)}
          ($ icon {:name "arrow-counterclockwise" :size 16})
          "Reshuffle Flagged Decks")
        ($ :button.button.button-danger
          {:type "button" :disabled (not has-temporary?) :on-click #(dispatch :attack-deck/end-scenario)}
          ($ icon {:name "trash3-fill" :size 16})
          "End Scenario")))))
