(ns game-type-test
  (:require [cljs.test :refer-macros [deftest is]]
            [ogres.app.game-type :as game-type]))

(deftest test-grid-tool-id-round-trip
  (doseq [value [:square :hex-pointy :hex-flat
                 :iso-square :iso-hex-pointy :iso-hex-flat
                 :iso-square-vertical :iso-hex-pointy-vertical :iso-hex-flat-vertical]]
    (is (= (game-type/grid-type-from-tool-id (game-type/grid-tool-id value)) value)
        "grid-tool-id and grid-type-from-tool-id are inverses for every
         concrete :scene/grid-type value.")))

(deftest test-grid-type-from-tool-id-non-grid
  (is (nil? (game-type/grid-type-from-tool-id :unit/light))
      "Non-grid element ids aren't mistaken for grid ones.")
  (is (nil? (game-type/grid-type-from-tool-id :tool/measurement))
      "Other :tool/* elements that aren't grid layouts return nil too."))

(deftest test-pick-grid-type
  (is (= (game-type/pick-grid-type #{:tool/grid-hex-flat}) :hex-flat)
      "Picks the one available grid option.")
  (is (= (game-type/pick-grid-type #{:tool/grid-hex-flat :tool/grid-square}) :square)
      "Picks the first option in canonical order when several are available.")
  (is (= (game-type/pick-grid-type #{}) :square)
      "Falls back to :square (used only underneath 'no-grid mode', where
       it's hidden and unaligned) when no grid tool is enabled.")
  (is (= (game-type/pick-grid-type #{:unit/light}) :square)
      "Non-grid elements in the set don't count as an available grid option."))

(deftest test-grid-count
  (is (= (game-type/grid-count #{:tool/grid-square :tool/grid-hex-flat :unit/light}) 2)
      "Counts only the grid-layout elements in the set.")
  (is (= (game-type/grid-count #{}) 0))
  (is (= (game-type/grid-count #{:unit/light :tool/measurement}) 0)
      "Non-grid elements don't count."))

(deftest test-sanitize-enabled-elements
  (is (= (game-type/sanitize-enabled-elements #{:unit/light :tool/grid-square :not/real})
         #{:unit/light :tool/grid-square})
      "Drops any id not present in the registry -- imported files are
       untrusted input.")
  (is (= (game-type/sanitize-enabled-elements nil) #{})
      "Handles nil (e.g. a malformed/empty import) gracefully."))

(deftest test-sanitize-icon-overrides
  (is (= (game-type/sanitize-icon-overrides
          {:unit/light {:icon/url "https://example.com/a.svg"}
           :unit/aura {:icon/sprite-name "compass"}
           :not/real {:icon/url "https://example.com/b.svg"}
           :unit/dead {:icon/url 12345}})
         {:unit/light {:icon/url "https://example.com/a.svg"}
          :unit/aura {:icon/sprite-name "compass"}})
      "Keeps only entries keyed by a recognized element id whose link
       value is a well-shaped :icon/link (a string sprite-name or url);
       drops unrecognized keys and malformed values.")
  (is (= (game-type/sanitize-icon-overrides nil) {})))
