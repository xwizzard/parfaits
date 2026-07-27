(ns ogres.app.game-type.games.dnd5e
  "D&D 5e-specific game-type elements -- HP tracking, status conditions
   (the SRD condition list), and the d20 initiative roll. Demonstrates the
   plugin pattern this framework is built around: this namespace's
   `elements` map merges into the shared registry (see `ogres.app.game-type`)
   purely by being required there -- nothing in the core rendering layer
   (panel_initiative.cljs, scene_context_menu.cljs, scene.cljs) ever names
   this namespace or its element ids directly. Core code only looks for the
   presence of `:initiative-panel`/`:initiative-actions`/`:token-panel`/
   `:token-badge` keys on whichever elements happen to be enabled for the
   active game-type, so a totally different game (see
   `ogres.app.game-type.games.gloomhaven`) can supply its own genuinely
   different rules under the same mechanism.

   \"Initiative\" as a word belongs here, not in core -- it's the TTRPG-
   specific name for D&D's particular way of determining turn order (a d20
   roll). The base turn system core provides is generic \"round\"/\"turn\"
   vocabulary with a manually-assignable order; this namespace's
   :dnd5e/initiative-roll is what layers the dice mechanic on top of that,
   opt-in, without the base system knowing or caring."
  (:require [ogres.app.game-type.widgets :as widgets]
            [uix.core :refer [$]]))

(def ^:private conditions
  "The D&D 5e SRD condition list -- reused as both the token context menu's
   checklist vocabulary and the canvas badge icon vocabulary (a single
   source of truth, unlike the two separately hardcoded copies this
   replaced)."
  [{:value :blinded       :icon "eye-slash-fill"}
   {:value :charmed       :icon "arrow-through-heart-fill"}
   {:value :defeaned      :icon "ear-fill"}
   {:value :exhausted     :icon "moon-stars-fill"}
   {:value :frightened    :icon "black-cat"}
   {:value :grappled      :icon "fist"}
   {:value :incapacitated :icon "lock-fill"}
   {:value :invisible     :icon "incognito"}
   {:value :paralyzed     :icon "cobra"}
   {:value :petrified     :icon "gem"}
   {:value :poisoned      :icon "poison-bottle"}
   {:value :prone         :icon "tripwire"}
   {:value :restrained    :icon "cobweb"}
   {:value :stunned       :icon "stars"}
   {:value :unconscious   :icon "activity"}])

(defn ^:private random-rolls
  "A lazy infinite sequence of d20 rolls (1-20), each shuffled group
   locally unique among itself -- D&D's 'roll for initiative.' Moved here
   from ogres.app.events (which no longer has any opinion about dice
   ranges); the only caller is :dnd5e/initiative-roll's bulk action below."
  []
  (sequence (mapcat shuffle) (repeat (range 1 21))))

(def elements
  {:dnd5e/hp-tracker
   {:label "HP Tracker"
    :icon  "heart-fill"
    ;; Shares an :exclusive-group with gloomhaven/hp-tracker -- see
    ;; ogres.app.game-type/exclusive-group. Cosmetically the same widget,
    ;; but a single game-type should never have two competing "the" HP
    ;; trackers enabled at once, so enabling either one here disables the
    ;; other automatically.
    :exclusive-group :hp-tracker
    :initiative-panel
    {:render
     (fn [{:keys [entity dispatch]}]
       ($ widgets/form-hp
         {:value (:initiative/health entity)
          :on-change
          (fn [f v] (dispatch :initiative/change-health (:db/id entity) f v))}))}}

   :dnd5e/conditions
   {:label "Conditions"
    :icon  "arrow-through-heart-fill"
    :token-panel
    {:icon    "arrow-through-heart-fill"
     :tooltip "Conditions"
     :render
     (fn [props]
       ($ widgets/status-checklist (assoc props :vocabulary conditions)))}
    :token-badge {:vocabulary conditions}}

   :dnd5e/initiative-roll
   {:label "Initiative Roll"
    :icon  "dice-5-fill"
    ;; Per-token: a small trigger additive to the generic rank widget
    ;; every game-type already has (see panel_initiative.cljs) -- this
    ;; doesn't replace manual entry/nudging, it's just a faster way to
    ;; fill the same :initiative/rank value with a d20 roll.
    :initiative-panel
    {:render
     (fn [{:keys [entity dispatch]}]
       ($ :button.initiative-token-roll-trigger
         {:type "button"
          :data-tooltip "Roll Initiative"
          :on-click #(dispatch :initiative/change-rank (:db/id entity) (inc (rand-int 20)))}
         ($ widgets/icon {:name "dice-5-fill" :size 16})))}
    ;; Footer/bulk-level: rolls a d20 for every eligible (non-player,
    ;; not-yet-ranked) participant at once, via the generic batch-write
    ;; event -- one transaction, not N. Eligibility is the shared
    ;; widgets/unranked-npc? predicate, not anything D&D-specific.
    :initiative-actions
    {:render
     (fn [{:keys [dispatch tokens]}]
       (let [eligible (into [] (filter widgets/unranked-npc?) tokens)]
         ($ :button.button.button-neutral
           {:type "button"
            :disabled (empty? eligible)
            :style {:text-transform "none"}
            :on-click
            (fn []
              (dispatch :initiative/assign-ranks
                        (zipmap (map :db/id eligible) (random-rolls))))}
           ($ widgets/icon {:name "dice-5-fill" :size 16}) "Roll Initiative for NPCs")))}}

   ;; A pure gate flag, same shape as :old-maid/game -- checked live by
   ;; component/panel_dice.cljs (via :game-type/enabled-elements) to
   ;; unlock advantage/disadvantage and per-player roll ownership on top
   ;; of the generic :tool/dice primitive (see ogres.app.dice and
   ;; events.cljs's :dice/roll), rather than replacing it with a
   ;; separate D&D-only roller. Distinct from :dnd5e/initiative-roll,
   ;; which stays exactly what it always was -- a d20-only shortcut for
   ;; filling :initiative/rank, untouched by this.
   :dnd5e/dice-roller
   {:label "Advantage/Disadvantage & Per-Player Rolls"
    :icon  "dice-5-fill"}})
