(ns ogres.app.game-type.games.gloomhaven
  "Gloomhaven-specific game-type elements. HP tracking reuses the exact same
   generic `widgets/form-hp` widget dnd5e uses -- proving that a
   cosmetically identical mechanic (a number with +/-/=) can still be an
   entirely separate, independently-owned registration, free to diverge
   later without touching D&D's file or the core framework. Status effects,
   on the other hand, are genuinely different from D&D's conditions: a
   different named vocabulary entirely (Gloomhaven has no 'blinded' or
   'petrified', and D&D has no 'muddle' or 'strengthen'), even though both
   render through the exact same :token-badge/:token-panel mechanism."
  (:require [ogres.app.game-type.widgets :as widgets]
            [uix.core :refer [$]]))

(def ^:private status-effects
  "Gloomhaven's status effect icons -- a different named vocabulary from
   D&D's SRD conditions (see ogres.app.game-type.games.dnd5e), under the
   same cosmetically-similar 'badge icons on a token' mechanism. Each
   icon is purpose-built original artwork (see web/release/icons.svg's
   'condition-*' symbols) rather than a generic Bootstrap-style glyph.

   No entry carries a :color, so the glyph itself is always a monotone
   silhouette inheriting the surrounding text color -- white on the canvas
   badge, current text color in the panel checklist. An earlier pass
   colored the GLYPH instead and it failed badly: at the 8px the canvas
   badge actually renders (see component/scene's token), a colored glyph
   on an arbitrarily-colored token is mush, and the darkest few vanished
   into the app's own dark chrome entirely.

   :badge-color is that same accent moved to where it does work -- it
   fills the badge's diamond BEHIND the white glyph, so color identifies
   the effect at a glance while the glyph keeps full white-on-dark
   contrast. It is deliberately not :color: these two would otherwise
   fight over the same 8px, which is exactly the mistake above. Panel
   checklist icons ignore it entirely. Values are sampled from the
   Frosthaven reference art each glyph was traced from."
  [{:value :bane        :icon "condition-bane"        :badge-color "#222325"}
   {:value :bless       :icon "condition-bless"       :badge-color "#d3a226"}
   {:value :brittle     :icon "condition-brittle"     :badge-color "#2798a3"}
   {:value :curse       :icon "condition-curse"       :badge-color "#7e58a6"}
   {:value :disarm      :icon "condition-disarm"      :badge-color "#68787d"}
   {:value :immobilize  :icon "condition-immobilize"  :badge-color "#9a322d"}
   {:value :impair      :icon "condition-impair"      :badge-color "#7c3649"}
   {:value :invisible   :icon "condition-invisible"   :badge-color "#131413"}
   {:value :muddle      :icon "condition-muddle"      :badge-color "#725945"}
   {:value :poison      :icon "condition-poison"      :badge-color "#7c8167"}
   {:value :regenerate  :icon "condition-regenerate"  :badge-color "#c73b96"}
   {:value :strengthen  :icon "condition-strengthen"  :badge-color "#4a98d4"}
   {:value :stun        :icon "condition-stun"        :badge-color "#2b4265"}
   {:value :ward        :icon "condition-ward"        :badge-color "#ce89b5"}
   {:value :wound       :icon "condition-wound"       :badge-color "#e56225"}])

(def elements
  {:gloomhaven/hp-tracker
   {:label "HP Tracker"
    :icon  "heart-fill"
    ;; Shares an :exclusive-group with dnd5e/hp-tracker -- see
    ;; ogres.app.game-type/exclusive-group. Same widget, but a single
    ;; game-type should never have two competing "the" HP trackers
    ;; enabled at once, so enabling either one here disables the other
    ;; automatically.
    :exclusive-group :hp-tracker
    :initiative-panel
    {:render
     (fn [{:keys [entity dispatch]}]
       ($ widgets/form-hp
         {:value (:initiative/health entity)
          :on-change
          (fn [f v] (dispatch :initiative/change-health (:db/id entity) f v))}))}}

   :gloomhaven/status-effects
   {:label "Status Effects"
    :icon  "stars"
    :token-panel
    {:icon    "stars"
     :tooltip "Status Effects"
     :render
     (fn [props]
       ($ widgets/status-checklist (assoc props :vocabulary status-effects)))}
    ;; :shape is a property of the vocabulary as a whole, not of any one
    ;; entry -- Gloomhaven/Frosthaven draw every status on a diamond, so
    ;; declaring it once here beats repeating it fifteen times. Other
    ;; game-types simply omit it and keep the default circle; component/
    ;; scene reads the key generically and still names no game.
    :token-badge {:vocabulary status-effects :shape :diamond}}

   ;; Gates the Attack Modifier Decks panel/tab -- each player's own
   ;; personal 20-card deck plus one shared monster deck, drawn from
   ;; instead of rolling a die (see ogres.app.attack-deck and events.cljs's
   ;; :attack-deck/* methods). Named around the mechanic, not this module,
   ;; so a future Frosthaven module could register its own element reusing
   ;; the same pure logic/events/schema -- the same way multiple card
   ;; games already share ogres.app.cards.
   :gloomhaven/attack-deck
   {:label "Attack Modifier Decks"
    :icon  "suit-spade-fill"}})
