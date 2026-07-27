(ns ogres.app.game-type.core-elements
  "The base set of toggle-able game-type elements: per-unit UI elements,
   canvas-level gameplay tools, and gameplay-specific systems. This is the
   framework's built-in vocabulary -- a future namespace (e.g. one
   contributing Gloomhaven-specific elements) plugs into the same registry
   by being added to the merge in `ogres.app.game-type`, without touching
   this map, the toggle mechanism, or any existing element's wiring.

   Named `core-elements` rather than nesting an `elements` segment under
   this namespace's own path (e.g. `ogres.app.game-type.elements.core`) --
   `ogres.app.game-type` itself defines a top-level var named `elements`,
   and under the Closure/Node-style namespace-as-nested-object model this
   test build (and any non-ESM target) compiles to, a var and a child
   namespace segment sharing the same name collide: assigning the var
   clobbers the object the child namespace's exports live on. ESM builds
   don't show this (each namespace gets its own module bindings), so it's
   silent in the app build and only surfaces here -- flattening the name
   avoids the collision outright rather than relying on module-system
   specifics.

   Each key is a namespaced keyword; the namespace segment (`:unit`,
   `:tool`, `:system`) doubles as the element's category, so no separate
   category field is needed. Each value is `{:label \"...\" :icon \"...\"}`
   where `:icon` is a *default* sprite name (see `ogres.app.component/icon`)
   -- a game-type may override it per-element via `:game-type/icon-overrides`.
   `:reserved? true` marks an element with no wired UI yet (none of these
   are currently reserved -- see `ogres.app.game-type/elements` for how the
   seeded 'Default' game-type only enables a small subset of these by
   default, leaving the rest -- including all of these -- for specific
   game-type templates like D&D 5e/Gloomhaven to opt into).")

(def elements
  {:unit/size        {:label "Size"}
   :unit/light       {:label "Light" :icon "sun-fill"}
   :unit/aura        {:label "Aura" :icon "compass"}
   :unit/dead        {:label "Dead flag" :icon "skull"}
   ;; "Turn flag", not "Initiative flag" -- this marks whether a token
   ;; participates in the base turn tracker at all (see
   ;; ogres.app.game-type/default-enabled-elements and
   ;; component/panel_initiative.cljs), which is a generic round/turn
   ;; concept. "Initiative" is TTRPG-specific phrasing for how a game
   ;; determines turn order (e.g. D&D's d20 roll -- see
   ;; ogres.app.game-type.games.dnd5e/:dnd5e/initiative-roll) and is
   ;; reserved for that kind of module-owned label, not this core flag.
   :unit/initiative  {:label "Turn flag" :icon "hourglass-split"}
   :unit/player      {:label "Player flag" :icon "person-circle"}

   :tool/measurement {:label "Measurement" :icon "rulers"}
   ;; A second, independent measurement primitive -- counts whole grid
   ;; cells (hex- or square-aware, see ogres.app.geom/cell-distance)
   ;; instead of a real-world unit. Not exclusive with :tool/measurement:
   ;; a game-type can enable either, or both at once, and the ruler
   ;; (scene_draw.cljs's draw-ruler) shows whichever are on. Gloomhaven's
   ;; board doesn't use feet at all, so its seeded template enables this
   ;; one instead of :tool/measurement (see game_type.cljs).
   :tool/measurement-cells {:label "Cell Measurement" :icon "square-cell"}
   :tool/mask        {:label "Masking" :icon "eye-slash-fill"}
   :tool/shapes      {:label "Shape drawing" :icon "triangle"}
   ;; Gates the Decks panel/tab -- the generic card/deck system (see
   ;; ogres.app.cards, game-type/core-decks.cljs, and events.cljs's
   ;; :deck/* methods). Not enabled by any seeded template yet; proven by
   ;; a Standard 52-card deck any game-type can opt into via the Builder.
   :tool/cards       {:label "Cards" :icon "suit-spade-fill"}
   ;; Gates the Dice panel/tab -- a generic n-sided-die primitive (see
   ;; ogres.app.dice and events.cljs's :dice/* methods), independent of
   ;; any game-type-specific rolling (e.g. D&D 5e's own :dnd5e/dice-
   ;; roller, which layers advantage/disadvantage and per-player
   ;; ownership onto this same primitive rather than replacing it). Not
   ;; enabled by any seeded template yet except D&D 5e's own, which pulls
   ;; it in explicitly alongside its own element (see game_type.cljs's
   ;; dnd5e-enabled-elements).
   :tool/dice        {:label "Dice" :icon "dice-5-fill"}

   ;; One entry per concrete :scene/grid-type value (see
   ;; component/panel_scene.cljs's `options-grid-type`, which this mirrors
   ;; 1:1) rather than a coarse square/hex/iso grouping, so each grid
   ;; layout can be individually enabled or disabled for a game type.
   :tool/grid-square                  {:label "Square" :icon "square"}
   :tool/grid-hex-pointy               {:label "Hex (Pointy)" :icon "hexagon"}
   :tool/grid-hex-flat                 {:label "Hex (Flat)" :icon "hexagon-flat"}
   :tool/grid-iso-square               {:label "Iso Square" :icon "square"}
   :tool/grid-iso-hex-pointy           {:label "Iso Hex (Pointy)" :icon "hexagon"}
   :tool/grid-iso-hex-flat             {:label "Iso Hex (Flat)" :icon "hexagon-flat"}
   :tool/grid-iso-square-vertical      {:label "Iso Square (Vertical)" :icon "square"}
   :tool/grid-iso-hex-pointy-vertical  {:label "Iso Hex Pointy (Vertical)" :icon "hexagon"}
   :tool/grid-iso-hex-flat-vertical    {:label "Iso Hex Flat (Vertical)" :icon "hexagon-flat"}})
