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

   No entry carries a :color: these are monotone silhouettes that inherit
   the surrounding text color, the same convention dnd5e's conditions
   already follow. An earlier pass gave each one its own accent color,
   which cost far more legibility than it bought -- at the 8px the canvas
   badge actually renders (see component/scene's token), a colored glyph
   on an arbitrarily-colored token is mush, and the darkest few vanished
   into the app's own dark chrome entirely."
  [{:value :bane        :icon "condition-bane"}
   {:value :bless       :icon "condition-bless"}
   {:value :brittle     :icon "condition-brittle"}
   {:value :curse       :icon "condition-curse"}
   {:value :disarm      :icon "condition-disarm"}
   {:value :immobilize  :icon "condition-immobilize"}
   {:value :impair      :icon "condition-impair"}
   {:value :invisible   :icon "condition-invisible"}
   {:value :muddle      :icon "condition-muddle"}
   {:value :poison      :icon "condition-poison"}
   {:value :regenerate  :icon "condition-regenerate"}
   {:value :strengthen  :icon "condition-strengthen"}
   {:value :stun        :icon "condition-stun"}
   {:value :ward        :icon "condition-ward"}
   {:value :wound       :icon "condition-wound"}])

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
    :token-badge {:vocabulary status-effects}}

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
