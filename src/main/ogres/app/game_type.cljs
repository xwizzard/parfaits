(ns ogres.app.game-type
  "Aggregates the game-type element registry from every element-source
   namespace -- the framework's own bare elements (`core`) plus every
   compiled-in game module. Adding a new game means adding it to this
   merge -- no schema change, no change to the toggle mechanism, no change
   to any existing element's wiring, and no change to any core rendering
   file (see the `games.dnd5e`/`games.gloomhaven` namespaces themselves,
   and panel_initiative.cljs/scene_context_menu.cljs/scene.cljs for how
   they're consumed generically via the :initiative-panel/:token-panel/
   :token-badge keys an element may declare)."
  (:require [clojure.string :refer [capitalize]]
            [ogres.app.game-type.core-decks :as core-decks]
            [ogres.app.game-type.core-elements :as core]
            [ogres.app.game-type.games.dnd5e :as dnd5e]
            [ogres.app.game-type.games.gloomhaven :as gloomhaven]
            [ogres.app.game-type.games.go-fish :as go-fish]
            [ogres.app.game-type.games.memory :as memory]))

(def elements
  (merge core/elements dnd5e/elements gloomhaven/elements memory/elements go-fish/elements))

(def deck-definitions
  "Every registered card-deck template, keyed by its own id (e.g.
   :standard-52) -- gated behind the :tool/cards element (see
   core-elements.cljs), not any single game-type. A future game module
   contributing its own deck is added to this merge the same way a game
   module's `elements` map is added above, no other file changes needed
   -- proven by go-fish/deck-definitions here, the first one to do so."
  (merge core-decks/definitions go-fish/deck-definitions))

(def game-labels
  "Display name for each contributing game module's element-id namespace,
   used to group the Game Builder UI (see panel_game_type_builder.cljs).
   A module not listed here still works -- its namespace is title-cased as
   a fallback -- this only controls the nicer label."
  {"dnd5e" "D&D 5e"
   "gloomhaven" "Gloomhaven"
   "go-fish" "Go Fish"})

(defn game-label [namespace-str]
  (get game-labels namespace-str (capitalize namespace-str)))

(def category-grid-elements
  "For a module whose real-world game is tied to one specific map/grid
   type (Gloomhaven's board is always point-top hexagons -- there's no
   square or iso variant of it), the set of :tool/grid-* ids that
   module's Builder category checkbox should exclusively enable when
   checked -- see panel_game_type_builder.cljs's `editor`, which uses
   this to force every *other* grid-layout element off in the same
   action rather than just adding the preferred one alongside whatever
   was already on (via :game-type/toggle-category's disable-ids arg).
   A module not listed here has no such constraint; its category
   checkbox is a plain toggle with no grid side effects. Keyed the same
   way `game-labels` is -- the module's element-id namespace string."
  {"gloomhaven" #{:tool/grid-hex-pointy}})

(def category-excluded-elements
  "For a module whose real-world game doesn't have a mechanic a shared
   core primitive represents -- Gloomhaven has no per-token light-radius
   mechanic at all, unlike D&D's darkvision-driven one -- the set of
   ids that module's Builder category checkbox should force *off* when
   checked, on top of whatever `category-grid-elements` already excludes.
   Same `editor`/disable-ids mechanism, just not grid-specific. A module
   not listed here excludes nothing beyond its grid preference (if any)."
  {"gloomhaven" #{:unit/light}})

(def default-enabled-elements
  "The bare-minimum universal element set the seeded 'Default' game-type
   starts with -- grid layouts plus basic token bookkeeping flags. Richer
   mechanics (masking, measurement, shape drawing, per-token size/light/
   aura, and anything module-specific like HP tracking or status
   conditions) are opt-in per game-type, whether toggled manually in the
   Builder or pulled in by a curated template like 'D&D 5e'/'Gloomhaven'
   (see `dnd5e-enabled-elements`/`gloomhaven-enabled-elements` below)."
  (into #{:unit/dead :unit/player :unit/initiative}
        (filter #(re-find #"^grid-" (name %)))
        (keys core/elements)))

(def dnd5e-enabled-elements
  "The seeded 'D&D 5e' game-type's starting set -- the bare default (which
   already includes :unit/initiative, so the base turn tracker is on),
   plus the shared core primitives that game wants (measurement, masking,
   shape drawing, size/light/aura), plus every element dnd5e.cljs
   contributes -- including :dnd5e/initiative-roll, D&D's own opt-in d20
   mechanism for assigning that base system's turn order automatically
   (see ogres.app.game-type.games.dnd5e; the base order is otherwise
   manually assigned, see ogres.app.events/:initiative/move)."
  (into default-enabled-elements
        (concat [:tool/measurement :tool/mask :tool/shapes
                 :unit/size :unit/light :unit/aura]
                (keys dnd5e/elements))))

(def gloomhaven-enabled-elements
  "The seeded 'Gloomhaven' game-type's starting set. Deliberately narrower
   than D&D's -- no masking or aura, which aren't part of that game, and
   no per-token light radius either (unlike D&D's darkvision-driven one,
   Gloomhaven has no light mechanic at all -- see
   `category-excluded-elements` for how the Builder's category checkbox
   enforces this same exclusion if a user manually re-enables it later).
   Uses :tool/measurement-cells instead of :tool/measurement -- Gloomhaven's
   board doesn't use feet at all, it measures range in grid cells (see
   ogres.app.geom/cell-distance). The two measurement primitives aren't
   exclusive, so a custom game-type built from this one could still turn
   :tool/measurement back on alongside it. Also doesn't enable any
   auto-roll turn-order mechanism (D&D's :dnd5e/initiative-roll is
   D&D-only): Gloomhaven's own turn order comes from drawn initiative
   cards, not a d20 roll, so this template just leaves the base
   manually-assigned order as-is rather than pretending the d20 mechanic
   fits. The generic card/deck system now exists (:tool/cards, see
   core-elements.cljs, core-decks.cljs, and events.cljs's :deck/*
   methods) -- proven here by a Standard 52-card deck available to any
   game-type -- but this template doesn't enable it by default yet:
   Gloomhaven's own monster ability-deck *content* (and its
   reshuffle-on-a-flagged-card rule, layered on the
   `ogres.app.cards/needs-reshuffle?` seam) is still out of scope for this
   pass. Catan-style clockwise seating or Root's nested sub-turn groups
   remain open turn-order ideas too. Each could plug into the exact same
   :initiative-panel/:initiative-actions mechanism D&D's module already
   proves out -- demonstrating that two real games legitimately want
   different subsets of the same shared primitives, decided here in
   data, with no core file caring which."
  (into default-enabled-elements
        (concat [:tool/measurement-cells :unit/size]
                (keys gloomhaven/elements))))

(def memory-enabled-elements
  "The seeded 'Memory' game-type's starting set. Deliberately close to
   the bare default -- Memory has no use for the shared tactical-map
   primitives other templates opt into (measurement/masking/shapes/
   size/light/aura -- it's a fixed grid of face-down cards, not a
   token-based map) beyond the baseline grid/token flags every
   template starts with. Explicitly drops :unit/initiative, on by
   default for every other template: Memory has its own turn-order UI
   (panel_memory.cljs, driven by :scene/memory-*), and showing the
   generic Turn Order tab alongside it would just be a second,
   redundant 'whose turn is it' panel with nothing in it (Memory cards
   aren't tokens and never populate the initiative tracker)."
  (into (disj default-enabled-elements :unit/initiative) (keys memory/elements)))

(def go-fish-enabled-elements
  "The seeded 'Go Fish' game-type's starting set -- the bare default
   (Go Fish has no use for the tactical-map primitives either, same
   reasoning as Memory's own template) plus :go-fish/game and
   :go-fish/book-scoring, the classic 4-card 'books' ruleset out of
   the box -- deliberately NOT `(keys go-fish/elements)` wholesale,
   since that would also pull in :go-fish/pair-scoring and violate
   its own :exclusive-group with book-scoring. The three extra-turn/
   ask-anyone rule elements are left off by default -- classic Go Fish
   turn order (one ask per turn, miss or hit, always ask the next
   player) -- each independently toggleable later via the Builder.
   Also drops :unit/initiative, for the exact same reason Memory's
   template does: Go Fish has its own turn-order UI
   (panel_go_fish.cljs, driven by :scene/go-fish-*)."
  (into (disj default-enabled-elements :unit/initiative)
        [:go-fish/game :go-fish/book-scoring]))

(defn grid-tool-id
  "The :tool/grid-* element id for a :scene/grid-type value, e.g.
   :hex-pointy -> :tool/grid-hex-pointy. One entry per concrete grid
   layout (see game_type/core_elements.cljs)."
  [grid-type-value]
  (keyword "tool" (str "grid-" (name grid-type-value))))

(defn grid-type-from-tool-id
  "The inverse of `grid-tool-id` -- nil for any element id that isn't a
   :tool/grid-* entry."
  [element-id]
  (when (and (= (namespace element-id) "tool")
             (re-find #"^grid-" (name element-id)))
    (keyword (subs (name element-id) 5))))

(def grid-order
  "The canonical, deterministic order `pick-grid-type` searches in --
   matches the order grid options are presented in the Scene panel."
  [:tool/grid-square :tool/grid-hex-pointy :tool/grid-hex-flat
   :tool/grid-iso-square :tool/grid-iso-hex-pointy :tool/grid-iso-hex-flat
   :tool/grid-iso-square-vertical :tool/grid-iso-hex-pointy-vertical
   :tool/grid-iso-hex-flat-vertical])

(defn pick-grid-type
  "The first :scene/grid-type value (in `grid-order`) available under the
   given enabled-elements set, or :square as a harmless fallback shape
   (only ever used underneath 'no-grid mode', where the grid is hidden
   and unaligned regardless of its underlying type -- see
   :game-type/toggle-element)."
  [enabled-elements]
  (or (some #(when (contains? enabled-elements %) (grid-type-from-tool-id %)) grid-order)
      :square))

(defn grid-count
  "How many grid-layout elements are enabled in the given set."
  [enabled-elements]
  (count (filter grid-type-from-tool-id enabled-elements)))

(defn exclusive-group
  "The `:exclusive-group` value declared by the given element id, or nil
   if it doesn't declare one. Elements sharing the same non-nil group are
   mutually exclusive within a single game-type -- enabling one via
   :game-type/toggle-element (see events.cljs) automatically disables
   every other member of the same group. Cosmetically-similar-but-
   separate registrations like D&D 5e's and Gloomhaven's HP trackers use
   this: the Builder edits one flat set over the whole merged registry
   regardless of which game a given element 'belongs' to, so nothing else
   stops a game-type from mixing elements across modules -- this is what
   keeps genuinely incompatible pairs (two competing HP trackers, say)
   from both ending up enabled at once."
  [element-id]
  (:exclusive-group (get elements element-id)))

(defn ^:private resolve-exclusive-conflicts
  "Given a set of element ids, keeps at most one member of each non-nil
   :exclusive-group -- ids with no group pass through untouched. Ties are
   broken by sorting so the result is deterministic. Used by
   `sanitize-enabled-elements` so an imported .gametype.edn file can't
   smuggle in two mutually-exclusive elements (e.g. both HP trackers) by
   simply listing both under :enabled-elements."
  [ids]
  (into #{}
        (mapcat (fn [[group members]] (if (nil? group) members [(first (sort members))])))
        (group-by exclusive-group ids)))

(defn sanitize-enabled-elements
  "Filters an arbitrary (e.g. imported from a file) collection down to
   only recognized registry element ids, resolving any mutually-exclusive
   conflicts (see `exclusive-group`) the same way toggling one on would,
   as a set."
  [coll]
  (-> (into #{} (filter (set (keys elements))) coll)
      resolve-exclusive-conflicts))

(defn sanitize-icon-overrides
  "Filters an arbitrary (e.g. imported from a file) map down to entries
   keyed by a recognized registry element id, whose value is a valid
   :icon/link -- {:icon/sprite-name string} or {:icon/url string}."
  [m]
  (into {} (filter (fn [[k link]]
                      (and (contains? elements k)
                           (or (string? (:icon/sprite-name link))
                               (string? (:icon/url link))))))
        m))
