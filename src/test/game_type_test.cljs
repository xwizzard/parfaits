(ns game-type-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.set :as set]
            [ogres.app.game-type :as game-type]
            [ogres.app.game-type.core-elements :as core]
            [ogres.app.game-type.games.crazy-eights :as crazy-eights]
            [ogres.app.game-type.games.dnd5e :as dnd5e]
            [ogres.app.game-type.games.gloomhaven :as gloomhaven]
            [ogres.app.game-type.games.go-fish :as go-fish]
            [ogres.app.game-type.games.memory :as memory]
            [ogres.app.game-type.games.old-maid :as old-maid]
            [ogres.app.game-type.games.rummy :as rummy]
            [ogres.app.game-type.games.war :as war]
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
        gloomhaven-ids (set (keys gloomhaven/elements))
        memory-ids (set (keys memory/elements))
        go-fish-ids (set (keys go-fish/elements))
        old-maid-ids (set (keys old-maid/elements))
        crazy-eights-ids (set (keys crazy-eights/elements))
        rummy-ids (set (keys rummy/elements))
        war-ids (set (keys war/elements))
        sources [core-ids dnd5e-ids gloomhaven-ids memory-ids go-fish-ids old-maid-ids crazy-eights-ids rummy-ids war-ids]]
    (is (every? empty? (for [a sources b sources :when (not= a b)] (set/intersection a b)))
        "every pair of sources contributes disjoint element ids -- any
         combination of game modules can be compiled in together
         without ever colliding.")
    (is (= (count game-type/elements) (apply + (map count sources)))
        "The merged registry has exactly as many entries as its nine
         sources combined -- nothing silently overwritten.")))

(deftest test-memory-registry-entry
  (is (= (get-in game-type/elements [:memory/game :label]) "Memory"))
  (is (string? (get-in game-type/elements [:memory/game :icon])))
  (is (contains? game-type/memory-enabled-elements :memory/game)
      "the seeded Memory template actually enables its own element")
  (is (not (contains? game-type/memory-enabled-elements :unit/initiative))
      "Memory has its own turn-order UI (panel_memory.cljs) -- the
       generic Turn Order tab is deliberately excluded to avoid a
       second, redundant 'whose turn is it' panel"))

(deftest test-go-fish-registry-entry
  (is (= (get-in game-type/elements [:go-fish/game :label]) "Go Fish"))
  (is (string? (get-in game-type/elements [:go-fish/game :icon])))
  (is (contains? game-type/go-fish-enabled-elements :go-fish/game)
      "the seeded Go Fish template actually enables its own tab-gating element")
  (is (contains? game-type/go-fish-enabled-elements :go-fish/book-scoring)
      "classic 4-card book scoring is the default out of the box")
  (is (not (contains? game-type/go-fish-enabled-elements :go-fish/pair-scoring))
      "book-scoring/pair-scoring are mutually exclusive -- only one is
       ever enabled by default")
  (is (not-any? game-type/go-fish-enabled-elements
                [:go-fish/extra-turn-on-hit :go-fish/extra-turn-on-lucky-draw :go-fish/ask-anyone])
      "the three optional rule variants are off by default -- classic
       Go Fish turn order out of the box")
  (is (not (contains? game-type/go-fish-enabled-elements :unit/initiative))
      "Go Fish has its own turn-order UI (panel_go_fish.cljs) -- the
       generic Turn Order tab is deliberately excluded")
  (is (= (get-in game-type/deck-definitions [:go-fish-9 :deck/name]) "Go Fish (9 Ranks)"))
  (let [cards (get-in game-type/deck-definitions [:go-fish-9 :deck/cards])]
    (is (= (count cards) 36) "9 ranks x 4 copies each = 36 cards")
    (is (= (count (into #{} (map :card/rank) cards)) 9) "9 distinct ranks")
    (is (every? #(= 4 %) (vals (frequencies (map :card/rank cards))))
        "exactly 4 copies of every rank")
    (is (not-any? :card/suit cards) "Go Fish matches by rank alone -- no suits")))

(deftest test-go-fish-exclusive-scoring-group
  (is (= (game-type/exclusive-group :go-fish/pair-scoring)
         (game-type/exclusive-group :go-fish/book-scoring))
      "pair-scoring and book-scoring share an :exclusive-group -- enabling
       one via :game-type/toggle-element automatically disables the other")
  (is (some? (game-type/exclusive-group :go-fish/pair-scoring))))

(deftest test-old-maid-registry-entry
  (is (= (get-in game-type/elements [:old-maid/game :label]) "Old Maid"))
  (is (string? (get-in game-type/elements [:old-maid/game :icon])))
  (is (contains? game-type/old-maid-enabled-elements :old-maid/game)
      "the seeded Old Maid template actually enables its own tab-gating element")
  (is (not (contains? game-type/old-maid-enabled-elements :unit/initiative))
      "Old Maid has its own turn-order UI (panel_old_maid.cljs) -- the
       generic Turn Order tab is deliberately excluded")
  (is (= (get-in game-type/deck-definitions [:old-maid-52 :deck/name]) "Old Maid"))
  (let [cards (get-in game-type/deck-definitions [:old-maid-52 :deck/cards])
        queens (filter (comp #{:queen} :card/rank) cards)]
    (is (= (count cards) 49) "the traditional 52 minus 3 of the 4 queens")
    (is (= (count queens) 1) "exactly 1 queen survives, reskinned")
    (is (= (:card/label (first queens)) "Old Maid"))
    (is (= (:card/icon (first queens)) "skull"))
    (is (every? #(= 4 %) (vals (dissoc (frequencies (map :card/rank cards)) :queen)))
        "every non-queen rank keeps all 4 suits -- 12 ranks x 4 = 48 pairing cards")))

(deftest test-crazy-eights-registry-entry
  (is (= (get-in game-type/elements [:crazy-eights/game :label]) "Crazy 8s"))
  (is (string? (get-in game-type/elements [:crazy-eights/game :icon])))
  (is (contains? game-type/crazy-eights-enabled-elements :crazy-eights/game)
      "the seeded Crazy 8s template actually enables its own tab-gating element")
  (is (not (contains? game-type/crazy-eights-enabled-elements :unit/initiative))
      "Crazy 8s has its own turn-order UI (panel_crazy_eights.cljs) -- the
       generic Turn Order tab is deliberately excluded")
  (is (= (get-in game-type/deck-definitions [:crazy-eights-52 :deck/name]) "Crazy 8s"))
  (let [cards (get-in game-type/deck-definitions [:crazy-eights-52 :deck/cards])
        eights (filter (comp #{:eight} :card/rank) cards)
        non-eights (remove (comp #{:eight} :card/rank) cards)]
    (is (= (count cards) 52) "the full traditional deck -- no cards removed")
    (is (= (count eights) 4) "all four 8s survive, just re-skinned")
    (is (every? #(= (:card/icon %) "star") eights)
        "every 8 shares the wild 'star' icon instead of its suit's")
    (is (every? #(= (:card/icon-color %) "var(--color-yellow-500)") eights)
        "the wild star is colored, unlike every other card's default
         (inherited) icon color")
    (is (not-any? :card/suit eights)
        "8s are suit-less wilds -- the whole point of the deck change")
    (is (every? :card/suit non-eights)
        "every OTHER card keeps its normal suit -- only the 8s are special-cased")))

(deftest test-rummy-registry-entry
  (is (= (get-in game-type/elements [:rummy/game :label]) "Rummy"))
  (is (string? (get-in game-type/elements [:rummy/game :icon])))
  (is (contains? game-type/rummy-enabled-elements :rummy/game)
      "the seeded Rummy template actually enables its own tab-gating element")
  (is (not (contains? game-type/rummy-enabled-elements :rummy/runs))
      "runs are a real, independently-toggleable rule variant -- off by
       default, unlike Old Maid/Crazy 8s' rulesets which had no real
       variants to invent toggles for")
  (is (not (contains? game-type/rummy-enabled-elements :unit/initiative))
      "Rummy has its own turn-order UI (panel_rummy.cljs) -- the
       generic Turn Order tab is deliberately excluded")
  (is (not (contains? game-type/deck-definitions :rummy))
      "Rummy contributes no deck-definitions entry at all -- it reuses
       :standard-52 completely unmodified, the purest reuse case yet"))

(deftest test-war-registry-entry
  (is (= (get-in game-type/elements [:war/game :label]) "War"))
  (is (string? (get-in game-type/elements [:war/game :icon])))
  (is (contains? game-type/war-enabled-elements :war/game)
      "the seeded War template actually enables its own tab-gating element")
  (is (not (contains? game-type/war-enabled-elements :unit/initiative))
      "War has no per-token turn order at all -- everyone plays
       simultaneously -- but :unit/initiative is still dropped, same
       reasoning as every other card game's template")
  (is (not (contains? game-type/deck-definitions :war))
      "War contributes no deck-definitions entry either -- reuses
       :standard-52 completely unmodified, same reuse tier as Rummy"))

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
