(ns game-type-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.set :as set]
            [ogres.app.game-type :as game-type]
            [ogres.app.game-type.core-elements :as core]
            [ogres.app.game-type.games.dnd5e :as dnd5e]
            [ogres.app.game-type.games.gloomhaven :as gloomhaven]
            [ogres.app.game-type.widgets :as widgets]))

(deftest test-measurement-cells-registry-entry
  (is (= (get-in core/elements [:tool/measurement-cells :label]) "Cell Measurement"))
  (is (string? (get-in core/elements [:tool/measurement-cells :icon])))
  (is (nil? (game-type/exclusive-group :tool/measurement-cells))
      ":tool/measurement and :tool/measurement-cells aren't mutually exclusive --
       a game-type can enable either, or both at once."))

(deftest test-cards-registry-entry
  (is (= (get-in core/elements [:tool/cards :label]) "Cards"))
  (is (string? (get-in core/elements [:tool/cards :icon]))))

(deftest test-deck-definitions
  (is (contains? game-type/deck-definitions :standard-52))
  (let [standard (get game-type/deck-definitions :standard-52)]
    (is (= (count (:deck/cards standard)) 52))
    (is (= (count (into #{} (:deck/cards standard))) 52)
        "all 52 cards are distinct (rank+suit combinations don't repeat)")
    (is (= (count (:deck/extras standard)) 2)
        "the standard deck's extras are its 2 jokers")))

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
      "Handles nil (e.g. a malformed/empty import) gracefully.")
  (let [result (game-type/sanitize-enabled-elements
                #{:unit/light :dnd5e/hp-tracker :gloomhaven/hp-tracker})]
    (is (contains? result :unit/light)
        "Elements with no :exclusive-group are untouched.")
    (is (= (count (filter #{:dnd5e/hp-tracker :gloomhaven/hp-tracker} result)) 1)
        "An imported file listing both mutually-exclusive HP trackers gets
         resolved down to just one, the same invariant toggling
         maintains interactively.")))

(deftest test-exclusive-group
  (is (= (game-type/exclusive-group :dnd5e/hp-tracker) :hp-tracker))
  (is (= (game-type/exclusive-group :gloomhaven/hp-tracker) :hp-tracker)
      "D&D 5e's and Gloomhaven's HP trackers share an :exclusive-group --
       cosmetically the same widget, but genuinely separate registrations
       that should never both be enabled on one game-type.")
  (is (nil? (game-type/exclusive-group :unit/light))
      "Elements with no declared :exclusive-group return nil.")
  (is (nil? (game-type/exclusive-group :not/real))
      "Unrecognized ids return nil rather than throwing."))

(deftest test-category-excluded-elements
  (is (= (game-type/category-excluded-elements "gloomhaven") #{:unit/light})
      "Gloomhaven has no per-token light-radius mechanic at all -- unlike
       D&D's darkvision-driven one -- so its Builder category checkbox
       (see panel_game_type_builder.cljs's `editor`) force-disables
       :unit/light when checked, same mechanism as its grid preference.")
  (is (nil? (game-type/category-excluded-elements "dnd5e"))
      "A module not listed excludes nothing beyond its grid preference
       (if any) -- D&D 5e's category checkbox has no extra exclusions."))

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

(deftest test-registry-merge-has-no-id-collisions
  (let [core-ids (set (keys core/elements))
        dnd5e-ids (set (keys dnd5e/elements))
        gloomhaven-ids (set (keys gloomhaven/elements))]
    (is (empty? (set/intersection core-ids dnd5e-ids))
        "core and dnd5e contribute disjoint element ids.")
    (is (empty? (set/intersection core-ids gloomhaven-ids))
        "core and gloomhaven contribute disjoint element ids.")
    (is (empty? (set/intersection dnd5e-ids gloomhaven-ids))
        "dnd5e and gloomhaven contribute disjoint element ids -- two game
         modules can be compiled in together without ever colliding.")
    (is (= (count game-type/elements)
           (+ (count core-ids) (count dnd5e-ids) (count gloomhaven-ids)))
        "The merged registry has exactly as many entries as its three
         sources combined -- nothing silently overwritten.")))

(deftest test-initiative-panel-lookup-path
  (let [element (:dnd5e/hp-tracker game-type/elements)]
    (is (fn? (get-in element [:initiative-panel :render]))
        "An element declaring :initiative-panel supplies a render fn that
         panel_initiative.cljs can call generically, by presence alone.")))

(deftest test-unranked-npc?
  (is (false? (widgets/unranked-npc? {:token/flags #{:player} :initiative/rank nil}))
      "A player token is never eligible, regardless of rank.")
  (is (false? (widgets/unranked-npc? {:token/flags #{} :initiative/rank 14}))
      "An NPC that already has a rank isn't eligible.")
  (is (true? (widgets/unranked-npc? {:token/flags #{} :initiative/rank nil}))
      "An NPC with no rank yet is exactly the eligible case."))

(deftest test-initiative-actions-lookup-path
  (let [element (:dnd5e/initiative-roll game-type/elements)]
    (is (fn? (get-in element [:initiative-panel :render]))
        "The per-token roll trigger is an :initiative-panel contribution,
         same as HP tracker -- discovered by panel_initiative.cljs's
         `token` the same generic way.")
    (is (fn? (get-in element [:initiative-actions :render]))
        "The bulk 'Roll Initiative for NPCs' button is an
         :initiative-actions contribution, discovered by
         panel_initiative.cljs's `actions` footer generically.")
    (is (not (contains? game-type/elements :system/initiative-roll))
        "The old core-owned placeholder is gone -- the d20 mechanic now
         lives entirely under this D&D-owned id.")))

(deftest test-token-panel-lookup-path
  (let [element (:dnd5e/conditions game-type/elements)]
    (is (string? (get-in element [:token-panel :icon])))
    (is (string? (get-in element [:token-panel :tooltip])))
    (is (fn? (get-in element [:token-panel :render]))
        "An element declaring :token-panel supplies its own toolbar
         icon/tooltip and a render fn, discovered by
         scene_context_menu.cljs iterating enabled elements generically.")))

(deftest test-token-badge-lookup-path
  (let [dnd5e-vocab (get-in (:dnd5e/conditions game-type/elements) [:token-badge :vocabulary])
        gloomhaven-vocab (get-in (:gloomhaven/status-effects game-type/elements) [:token-badge :vocabulary])]
    (is (every? (fn [{:keys [value icon]}] (and (keyword? value) (string? icon))) dnd5e-vocab)
        "D&D 5e's :token-badge vocabulary is well-shaped {:value :icon} data.")
    (is (every? (fn [{:keys [value icon]}] (and (keyword? value) (string? icon))) gloomhaven-vocab)
        "Gloomhaven's :token-badge vocabulary is well-shaped {:value :icon} data.")
    (is (not= (set (map :value dnd5e-vocab)) (set (map :value gloomhaven-vocab)))
        "The two games genuinely have different status vocabularies under
         the same cosmetically-similar :token-badge mechanism.")))
