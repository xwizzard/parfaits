(ns ogres.app.component.panel-dice
  "The 'Dice' panel/tab -- a minimal UI over events.cljs's :dice/*
   methods and ogres.app.dice's pure roll/combine logic. Gated by the
   generic :tool/dice element (see panel.cljs's visible-tabs), visible
   to host and guest alike in Play mode only.

   The universal primitive (:tool/dice alone) is deliberately bare: a
   die-pool builder (any mix of the 7 standard sizes) plus a Roll
   button, every roll shared/neutral -- no owner, no advantage/
   disadvantage. When the D&D 5e game-type's own :dnd5e/dice-roller
   element is ALSO enabled (checked live against :game-type/enabled-
   elements, the same pattern Go Fish's/Rummy's own optional rule
   toggles already use), this SAME form grows two extra controls: a
   Normal/Advantage/Disadvantage mode picker and a 'roll as' player
   select -- D&D doesn't get a separate roller, it unlocks more of
   this one."
  (:require [ogres.app.component :refer [icon]]
            [ogres.app.hooks :as hooks]
            [ogres.app.player :as player]
            [uix.core :as uix :refer [defui $]]))

(def ^:private standard-sides
  "The 7 standard D&D die sizes the picker curates the primitive down
   to -- events.cljs's own :dice/roll accepts any positive integer,
   this is a UI choice, not a limit of the underlying primitive."
  [4 6 8 10 12 20 100])

(def ^:private query
  [{:root/user
    [:user/uuid
     :user/host
     {:user/camera
      [{:camera/scene
        [{:scene/game-type [[:game-type/enabled-elements :default #{}]]}
         {:scene/dice-rolls
          [:db/id
           [:roll/dice :default nil]
           [:roll/mode :default nil]
           [:roll/result :default nil]
           [:roll/rolled-at :default 0]
           {:roll/owner [:db/id :player/name :player/color]}]}]}]}]}
   {:root/players
    [:db/id :player/name :player/kind [:player/active :default true]
     {:player/controller [:user/uuid]}]}
   {:root/session [{:session/conns [:user/uuid]}]}])

(defn ^:private dice-state
  "Derived Dice state `panel`/`actions` both need, pulled once per
   render via the shared `query` above."
  [result]
  (let [{uuid :user/uuid host :user/host
         {scene :camera/scene} :user/camera} (:root/user result)
        enabled (:game-type/enabled-elements (:scene/game-type scene))
        connected (into #{} (map :user/uuid) (:session/conns (:root/session result)))
        ;; Only players the viewer actually has authority over (their
        ;; own controlled seat, or any host-fallback-eligible
        ;; unassigned one) are offered in the "roll as" picker -- the
        ;; event itself re-checks this authoritatively (see :dice/
        ;; roll's own docstring), this is just so the dropdown doesn't
        ;; offer choices that would silently no-op.
        rollable-players
        (filter (fn [p] (player/authority? uuid host connected (get-in p [:player/controller :user/uuid])))
                (:root/players result))]
    {:host host
     :dnd-dice? (contains? enabled :dnd5e/dice-roller)
     :rollable-players rollable-players
     :rolls (sort-by :roll/rolled-at > (:scene/dice-rolls scene))}))

(defui ^:private die-pool-form
  [{:keys [dnd-dice? rollable-players on-roll]}]
  (let [[pool set-pool] (uix/use-state {})
        [mode set-mode] (uix/use-state nil)
        [owner set-owner] (uix/use-state "")
        total (apply + (vals pool))]
    ($ :form.dice-pool-form
      {:on-submit
       (fn [event]
         (.preventDefault event)
         (let [sides-seq (mapcat (fn [[sides n]] (repeat n sides)) pool)]
           (if (seq sides-seq)
             (do (on-roll sides-seq mode (if (seq owner) (js/Number owner) nil))
                 (set-pool {})))))}
      ($ :fieldset.fieldset.dice-pool-rows
        ($ :legend "Dice")
        (for [sides standard-sides]
          ($ :.dice-pool-row {:key sides}
            ($ :label.dice-pool-label (str "d" sides))
            ($ :input.text.dice-pool-count
              {:type "number" :min 0 :max 20
               :value (get pool sides 0)
               :on-change
               (fn [event]
                 (let [n (max 0 (min 20 (js/Number (.. event -target -value))))]
                   (set-pool (fn [p] (if (zero? n) (dissoc p sides) (assoc p sides n))))))}))))
      (if dnd-dice?
        ($ :fieldset.fieldset.dice-mode-row
          ($ :legend "Mode")
          (for [[value label] [[nil "Normal"] [:advantage "Advantage"] [:disadvantage "Disadvantage"]]]
            ($ :label.radio {:key (str value)}
              ($ :input
                {:type "radio"
                 :name "dice-mode"
                 :checked (= mode value)
                 :on-change
                 (fn []
                   (set-mode value)
                   ;; Advantage/disadvantage with a single die selected
                   ;; conveniently bumps it to 2 -- the combining logic
                   ;; (ogres.app.dice/best,worst) works over however
                   ;; many dice are actually in the pool regardless,
                   ;; this just saves a click for the common case of
                   ;; "roll this one die with advantage."
                   (if (and value (= total 1))
                     (set-pool (fn [p] (into {} (map (fn [[s n]] [s (* n 2)])) p)))))})
              label))))
      (if dnd-dice?
        ($ :select.dice-owner-select
          {:value owner :on-change #(set-owner (.. % -target -value))}
          ($ :option {:value ""} "Shared (no owner)")
          (for [p rollable-players]
            ($ :option {:key (:db/id p) :value (:db/id p)} (:player/name p)))))
      ($ :button.button.button-neutral
        {:type "submit" :disabled (zero? total)}
        ($ icon {:name "dice-5-fill" :size 16})
        "Roll"))))

(defui ^:private roll-item [{:keys [roll]}]
  (let [{dice :roll/dice mode :roll/mode result :roll/result owner :roll/owner} roll]
    ($ :li.dice-roll-item
      ($ :.dice-roll-header
        ($ :span.dice-roll-owner {:data-color (:player/color owner)}
          (if owner (:player/name owner) "Shared"))
        (if mode ($ :span.dice-roll-mode (name mode)))
        ($ :span.dice-roll-result result))
      ($ :.dice-roll-dice
        (for [[i {:keys [sides value]}] (map-indexed vector dice)]
          ($ :span.dice-roll-die
            {:key i :data-picked (boolean (and mode (= value result)))}
            "d" sides " → " value))))))

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        {:keys [dnd-dice? rollable-players rolls]} (dice-state result)]
    ($ :.form-dice
      ($ :header ($ :h2 "Dice"))
      ($ die-pool-form
        {:dnd-dice? dnd-dice?
         :rollable-players rollable-players
         :on-roll (fn [sides-seq mode owner-id] (dispatch :dice/roll sides-seq mode owner-id))})
      (if (seq rolls)
        ($ :ul.dice-roll-list
          (for [roll rolls]
            ($ roll-item {:key (:db/id roll) :roll roll})))
        ($ :.form-notice "No rolls yet -- build a pool above and roll.")))))

(defui ^:memo actions []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query [:db/ident :root])
        {:keys [host rolls]} (dice-state result)]
    (if host
      ($ :button.button.button-danger
        {:type "button" :disabled (empty? rolls) :on-click #(dispatch :dice/clear)}
        ($ icon {:name "trash3-fill" :size 16})
        "Clear History"))))
