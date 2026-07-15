(ns ogres.app.game-type
  "Aggregates the game-type element registry from every element-source
   namespace. Adding a new toggle-able element later (e.g. a future
   namespace contributing Gloomhaven-specific elements) means adding it to
   this merge -- no schema change, no change to the toggle mechanism, no
   change to any existing element's wiring."
  (:require [ogres.app.game-type.elements.core :as core]))

(def elements
  (merge core/elements))

(def default-enabled-elements
  "Every real (non-reserved) element, enabled -- the seeded 'Default'
   game-type's starting set, matching today's behavior exactly."
  (into #{} (comp (remove (comp :reserved? val)) (map key)) elements))

(defn grid-tool-id
  "The :tool/grid-* element id for a :scene/grid-type value, e.g.
   :hex-pointy -> :tool/grid-hex-pointy. One entry per concrete grid
   layout (see game_type/elements/core.cljs)."
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

(defn sanitize-enabled-elements
  "Filters an arbitrary (e.g. imported from a file) collection down to
   only recognized registry element ids, as a set."
  [coll]
  (into #{} (filter (set (keys elements))) coll))

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
