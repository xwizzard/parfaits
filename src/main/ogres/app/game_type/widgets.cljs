(ns ogres.app.game-type.widgets
  "Small, reusable UI primitives and data helpers available to any
   game-type module (see ogres.app.game-type.games.*) for building their
   :initiative-panel/:initiative-actions/:token-panel contributions.
   Deliberately independent of
   `ogres.app.component`/`ogres.app.hooks` -- those transitively require
   `ogres.app.events` (for the app's full dispatch/event-bus machinery),
   which itself requires `ogres.app.game-type`, and `game-type` requires
   every game module -- so a game module requiring anything on that chain
   would be a circular dependency. This namespace only ever requires
   `uix.core`/`clojure.string`/`ogres.app.const`, none of which reach back
   up into the app's event layer, keeping it a genuine leaf."
  (:require [clojure.string :refer [capitalize]]
            [ogres.app.const :refer [PATH]]
            [uix.core :as uix :refer [defui $]]))

(defui icon
  "A minimal local copy of ogres.app.component/icon (see its docstring) --
   duplicated rather than shared for the leaf-namespace reason above.
   Public (unlike the other private helpers here) so a game module can
   render its own icon buttons directly -- see e.g.
   ogres.app.game-type.games.dnd5e's initiative-roll triggers."
  [{:keys [name size] :or {size 22}}]
  ($ :svg
    {:fill "currentColor"
     :role "presentation"
     :class "icon"
     :width size
     :height size}
    ($ :use {:href (str PATH "/icons.svg" "#icon-" name)})))

(defn ^:private use-outside-click
  "A minimal local copy of ogres.app.hooks/use-modal's behavior (see its
   docstring) -- duplicated rather than shared for the leaf-namespace
   reason above."
  []
  (let [[state set-state] (uix/use-state false)
        ref (uix/use-ref)]
    (uix/use-effect
     (fn []
       (if state
         (let [handler
               (fn [event]
                 (if-let [node (deref ref)]
                   (if (not (.contains node (.-target event)))
                     (set-state false))))]
           (js/document.addEventListener "click" handler)
           (fn [] (js/document.removeEventListener "click" handler)))))
     [state])
    [state set-state ref]))

(defui form-hp
  "A generic heart-icon HP editor: shows the current value, and on click
   opens a small form with a numeric input plus subtract/add/set buttons.
   Game-agnostic -- any per-game HP-tracking component can wrap this,
   supplying its own `value`/`on-change` wired to whatever event that
   game's HP field uses (see e.g. ogres.app.game-type.games.dnd5e).
   ```
   ($ form-hp {:value 12 :on-change (fn [f v] (dispatch ... f v))})
   ```"
  [{:keys [value on-change]}]
  (let [[editing set-editing form] (use-outside-click)
        input (uix/use-ref)]
    ($ :.initiative-token-health
      {:data-present (some? value)}
      ($ :.initiative-token-health-frame
        ($ icon {:name "heart-fill" :size 40}))
      ($ :button.initiative-token-health-label
        {:on-click (fn [event] (.stopPropagation event) (set-editing not))}
        (or value "HP"))
      (if editing
        ($ :form.initiative-token-form
          {:ref form
           :data-type "health"
           :on-submit
           (fn []
             (on-change (fn [_ v] v) (.-value @input))
             (set-editing not))}
          ($ :input.text.text-ghost
            {:type "number"
             :name "hitpoints"
             :ref input
             :auto-focus true
             :placeholder "Hitpoints"
             :aria-label "Hitpoints"})
          (for [[key label f] [["-" "Subtract from" -] ["+" "Add to" +] ["=" "Set as" (fn [_ v] v)]]]
            ($ :button
              {:key key :type "button" :aria-label label
               :on-click
               (fn []
                 (on-change f (.-value @input))
                 (set-editing not))} key)))))))

(defui ^:private indeterminate-checkbox
  "A checkbox whose native `.indeterminate` DOM property (unavailable as a
   plain React/HTML attribute) is kept in sync with `checked` being the
   sentinel value :indeterminate -- used by status-checklist below to show
   a tri-state box across a multi-token selection. Takes a render-prop
   `children` function passed the input ref to attach to the actual
   `<input>` it renders."
  [{:keys [checked children]}]
  (let [input (uix/use-ref)
        indtr (= checked :indeterminate)]
    (uix/use-effect
     (fn [] (set! (.-indeterminate @input) indtr)) [indtr])
    (children input)))

(defui status-checklist
  "Renders `vocabulary` (a seq of {:value :icon :label} maps) as a list of
   tri-state status-flag checkboxes -- checked/unchecked/indeterminate
   across a multi-token selection -- each toggling its own :value in
   :token/flags via the given `on-change` (typically bound to
   :token/change-flag). Game-agnostic: this is the same UI shape D&D's
   conditions and Gloomhaven's status effects both use, just with a
   different vocabulary -- see ogres.app.game-type.games.dnd5e/gloomhaven.
   ```
   ($ status-checklist {:vocabulary [{:value :blinded :icon \"eye-slash-fill\"}]
                         :values values :on-change on-change})
   ```"
  [{:keys [vocabulary values on-change]
    :or   {values (constantly (list)) on-change identity}}]
  (let [fqs (frequencies (reduce into [] (values :token/flags [])))
        ids (values :db/id)]
    (for [{value :value icon-name :icon label :label} vocabulary
          :let [focus (= value (:value (first vocabulary)))
                state (cond (= (get fqs value 0) 0) false
                            (= (get fqs value 0) (count ids)) true
                            :else :indeterminate)]]
      ($ indeterminate-checkbox {:key value :checked state}
        (fn [input]
          ($ :label {:aria-label (name value) :data-tooltip (or label (capitalize (name value)))}
            ($ :input
              {:ref input
               :type "checkbox"
               :name (str "status-flag-" (name value))
               :checked (if (= state :indeterminate) false state)
               :auto-focus focus
               :on-change
               (fn [event]
                 (let [checked (.. event -target -checked)]
                   (on-change :token/change-flag value checked)))})
            ($ icon {:name icon-name})))))))

(defn unranked-npc?
  "True when the given (pulled) token has no :player flag and no
   :initiative/rank set yet -- i.e. a turn-tracker participant nothing
   has assigned an order to. Game-agnostic bulk-assignment eligibility
   logic (\"which participants still need an order\"), not tied to any
   particular way of assigning that order -- reusable by any module that
   wants an \"assign everyone at once\" action, see
   ogres.app.game-type.games.dnd5e's :initiative-actions contribution."
  [token]
  (and (not (contains? (:token/flags token) :player))
       (nil? (:initiative/rank token))))
