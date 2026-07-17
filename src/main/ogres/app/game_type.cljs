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
            [ogres.app.game-type.core-elements :as core]
            [ogres.app.game-type.games.dnd5e :as dnd5e]
            [ogres.app.game-type.games.gloomhaven :as gloomhaven]))

(def elements
  (merge core/elements dnd5e/elements gloomhaven/elements))

(def game-labels
  "Display name for each contributing game module's element-id namespace,
   used to group the Game Builder UI (see panel_game_type_builder.cljs).
   A module not listed here still works -- its namespace is title-cased as
   a fallback -- this only controls the nicer label."
  {"dnd5e" "D&D 5e"
   "gloomhaven" "Gloomhaven"})

(defn game-label [namespace-str]
  (get game-labels namespace-str (capitalize namespace-str)))

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
   than D&D's -- no masking or aura, which aren't part of that game.
   Also doesn't enable any auto-roll turn-order mechanism (D&D's
   :dnd5e/initiative-roll is D&D-only): Gloomhaven's own turn order comes
   from drawn initiative cards, not a d20 roll, so this template just
   leaves the base manually-assigned order as-is rather than pretending
   the d20 mechanic fits. Building Gloomhaven's own card-draw element (or
   Catan-style clockwise seating, or Root's nested sub-turn groups) is out
   of scope for this pass, but each could plug into the exact same
   :initiative-panel/:initiative-actions mechanism D&D's module already
   proves out -- demonstrating that two real games legitimately want
   different subsets of the same shared primitives, decided here in
   data, with no core file caring which."
  (into default-enabled-elements
        (concat [:tool/measurement :unit/size :unit/light]
                (keys gloomhaven/elements))))

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
