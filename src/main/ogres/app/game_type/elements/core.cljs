(ns ogres.app.game-type.elements.core
  "The base set of toggle-able game-type elements: per-unit UI elements,
   canvas-level gameplay tools, and gameplay-specific systems. This is the
   framework's built-in vocabulary -- a future namespace (e.g. one
   contributing Gloomhaven-specific elements) plugs into the same registry
   by being added to the merge in `ogres.app.game-type`, without touching
   this map, the toggle mechanism, or any existing element's wiring.

   Each key is a namespaced keyword; the namespace segment (`:unit`,
   `:tool`, `:system`) doubles as the element's category, so no separate
   category field is needed. Each value is `{:label \"...\" :icon \"...\"}`
   where `:icon` is a *default* sprite name (see `ogres.app.component/icon`)
   -- a game-type may override it per-element via `:game-type/icon-overrides`.
   `:reserved? true` marks an element with no wired UI yet.")

(def elements
  {:unit/size        {:label "Size"}
   :unit/light       {:label "Light" :icon "sun-fill"}
   :unit/aura        {:label "Aura" :icon "compass"}
   :unit/conditions  {:label "Conditions" :icon "arrow-through-heart-fill"}
   :unit/dead        {:label "Dead flag" :icon "skull"}
   :unit/initiative  {:label "Initiative flag" :icon "hourglass-split"}
   :unit/player      {:label "Player flag" :icon "person-circle"}

   :tool/measurement {:label "Measurement" :icon "rulers"}
   :tool/mask        {:label "Masking" :icon "eye-slash-fill"}
   :tool/shapes      {:label "Shape drawing" :icon "triangle"}

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
   :tool/grid-iso-hex-flat-vertical    {:label "Iso Hex Flat (Vertical)" :icon "hexagon-flat"}

   :system/initiative-roll {:label "Initiative (d20 roll)" :icon "dice-5-fill"}
   :system/hp-tracker      {:label "HP Tracker" :reserved? true}})
