# Feature discovery: inspiration survey

A survey of reference VTT and Gloomhaven-companion projects, cloned locally at
`~/Documents/Inspirations/` (not part of this repo — reference material only,
nothing here was copied verbatim). Purpose: a running list of ideas worth
considering for parfaits, with enough detail to pick back up later without
re-reading the source projects. Add to this doc as new ideas surface; move an
item to a real plan once it's actually being scoped.

Survey conducted 2026-07-21 across 8 parallel research passes. Source
directories referenced below are all under `~/Documents/Inspirations/`.

## Prioritized candidates (highest value first)

1. **Monster ability-deck system** (Gloomhaven's core mechanic — see
   `gloomycompanion` below). Nothing in parfaits models this yet. Data model
   is clean and portable; looks tractable as a new game-type element/slot.
2. **Monster stat-by-level table** (see `haven-keeper` below). Also entirely
   absent from `gloomhaven.cljs` today, and the reference data shape is
   close to ready-to-port.
3. **Gloomhaven condition badge set** — slots into the existing generalized
   `:token-badge` vocabulary (`scene.cljs`) with no new mechanism needed,
   just new data.
4. **Hex-ring vision** (exact N-hex-ring fog/sight boundary instead of a
   circular radius) — relevant if Gloomhaven-style limited vision is ever
   wanted; see FoundryVTT's `hexploration` below.
5. Smaller initiative/turn-order enhancements (grouped NPC rolls, richer
   tiebreak chains, timed-effect counters) — see the Initiative section.

Everything else below is lower-priority / informational — either validates a
decision parfaits already made, or is a bigger lift than its payoff justifies
right now.

## Networking & multiplayer architecture

Surveyed: Fari (`Fari/fari-app`, `Fari/fari-peer-server`), PlanarAlly
(`PlanarAlly/PlanarAlly`), Owlbear Rodeo legacy
(`OwlbearRodeo/owlbear-rodeo-legacy`).

- **Fari abandoned true P2P.** `fari-peer-server` (WebRTC signaling only) is
  now dead code — `fari-app` migrated entirely to a hosted realtime
  backend-as-a-service (Liveblocks): GM owns a `LiveObject` as canonical
  state, players subscribe. Room id is literally the GM's user id — a
  dead-simple addressing scheme worth remembering, but the storage model
  itself pushes CRDT semantics onto a third-party service, which doesn't fit
  parfaits' self-hosted approach.
- **Owlbear Rodeo legacy is full-mesh WebRTC** (`simple-peer`), with a
  Socket.IO server used *only* for signaling/relay — same shape as parfaits'
  own relay server. Notably, assets (map/token images) are never uploaded to
  a server at all; they live in each client's IndexedDB and stream
  peer-to-peer via an asset manifest tracking per-asset `owner`
  (`src/network/NetworkedMapAndTokens.tsx`). Interesting if a "no data at
  rest on the server" story ever matters, but a real lift (host-liveness
  dependency, P2P chunking/backpressure) for gains that are mostly about
  privacy/hosting cost, not better sync semantics.
- **PlanarAlly is the opposite extreme** — a fully authoritative
  Python/aiohttp+socketio server with a real SQL schema (peewee ORM,
  `server/src/db/models/*.py`, ~40 model files: shapes, floors, layers,
  initiative, trackers, auras all persisted). Clients reconnect and
  re-sync from the DB rather than depending on another client staying
  connected. Real tradeoff: much stronger persistence/crash-recovery and a
  natural place for server-side permission checks
  (`server/src/models/access.py`), at the cost of a much heavier server than
  parfaits has chosen to run.
- **Takeaway:** parfaits' relay-only WebSocket model (`src/main/ogres/server/core.clj`)
  sits at a reasonable, deliberate point in this design space — none of
  these three are a "just adopt this" case, they're validation that the
  space has real tradeoffs and parfaits picked a sensible one for a
  self-hosted hobby-scale tool.

## Plugin / extension architecture

Surveyed: FoundryVTT systems (`FoundryVTT/dnd5e`, `pf2e`, `crucible`,
`black-flag`, `worldbuilding`), Owlbear Rodeo's modern SDK
(`OwlbearRodeo/sdk`, `initiative-tracker`, `dynamic-fog`), Roll20 Beacon
sheets (`Roll20/roll20-beacon-sheets`).

- **FoundryVTT systems don't have anything like parfaits' key-presence
  element registry.** `dnd5e.mjs`'s `Hooks.once("init", ...)` just
  overwrites Foundry's global `CONFIG` object wholesale (document classes,
  lookup tables) — imperative replacement, not declarative merge.
  `system.json`'s `documentTypes` block (Actor/Item subtypes with per-type
  field metadata) is a lightweight manifest schema worth a glance if
  parfaits' own module manifests ever need per-type field declarations.
  `worldbuilding` (Foundry's no-ruleset starter system) is the closest
  analog to parfaits' "Default" game-type — single Actor type, free-form
  attribute bags, nothing structurally novel.
- **Owlbear Rodeo's *modern* SDK is the most genuinely different idea in
  the whole survey.** Extensions are separate web apps (own manifest, own
  iframe) talking to the host over `postMessage`. Registration points are
  generic UI surfaces (`contextMenu.create`, `action`, `popover`, `modal`,
  `tool`) filtered by declarative item-property matchers, not fixed named
  slots. Crucially: **everything is a generic `Item` plus a
  plugin-namespaced `metadata` bag** — their initiative tracker
  (`initiative-tracker/src/InitiativeTracker.tsx`) has *no dedicated data
  model at all*; it just stores
  `item.metadata["rodeo.owlbear.initiative-tracker/metadata"] = {count, active}`
  on existing scene items. This is a more ECS-like design than parfaits'
  dedicated schema attributes (`:initiative/rank` etc.) — worth an explicit
  design conversation at some point about whether future extensibility
  should piggyback on a metadata bag rather than growing core schema,
  even if the answer ends up being "no, keep the current approach, it's
  simpler for a single-codebase app without third-party extensions."
  `dynamic-fog`'s `Reconciler`/`Reactor`/`Actor` pattern (deriving local,
  unshared child items like fog polygons from the shared scene one-way) is
  also a clean pattern if parfaits' lighting ever grows beyond
  radius-only.
- **Roll20 Beacon sheets** (`roll20-beacon-sheets/dnd-ogl/src/`) have a
  `schemas/` directory (typed per-concept schemas: class, race, spell,
  feature, effect, resource, action, damage, npc) plus an
  `effects.config.ts` — a static-keys registry of composable modifier
  targets (`armor-class`, `melee-attack-modifier`, etc.) that
  features/items/conditions can hook into generically. A nice analog to
  parfaits' `:token-badge`/`:token-panel` element-key convention, but
  applied to numeric modifiers rather than UI slots — relevant only if
  parfaits ever needs cross-module modifier composition (e.g. "this
  condition modifies that stat"), not needed today.
- The old Roll20 `roll20-character-sheets` (monolithic HTML+autocalc
  formulas, no shared schema) is worth remembering only as the "legacy
  anti-pattern" parfaits' pluggable design already avoids.

## Grid & hex math

Surveyed: RPTools MapTool (`RPTools/maptool`), PlanarAlly, Owlbear Rodeo
legacy, FoundryVTT `hexploration`.

- **MapTool's `HexGrid.java` supports *irregular* hexes** via a `hexRatio`
  field (`vRadius/uRadius`) decoupled from the regular-hex assumption
  (`REGULAR_HEX_RATIO = √3/2`) — more general than parfaits' current
  fixed-ratio flat/pointy toggle. `IsometricGrid.java` does true
  diamond-projection coordinate conversion (`zoneToCell`/`cellToZone`)
  rather than skewing a square grid the way parfaits' iso grids currently
  do. `GridCapabilities.java` is a nice small pattern worth copying
  regardless of the math: each grid type declares which features it
  supports (pathing, snap-to-grid, coordinate display) instead of scattered
  grid-type branches elsewhere in the code.
- **FoundryVTT's `hexploration` module computes exact hex-shaped vision
  boundaries** — precomputed normalized hexagon polygon offsets for 1-hex
  and 2-hex sight rings (`module/canvas/hex-sight.mjs`), scaled by grid
  size, rather than a circular vision radius. Directly relevant if
  Gloomhaven-style "vision limited to N hexes" ever comes up — see
  prioritized candidate #4 above.
- **PlanarAlly's grid module** (`client/src/core/grid/hex.ts`) implements
  standard axial/cube hex rounding (redblobgames-style) shared between
  flat-top and pointy-top via an `isFlat` flag — solid reference
  implementation if parfaits' own hex math ever needs a second opinion,
  though parfaits' existing `vec/nearest-hex` already covers the same
  ground.
- **Owlbear Rodeo legacy's grid rendering** draws grid lines as a
  repeating `fillPatternImage` (a pre-rendered PNG tile) rather than
  vector-drawn lines — a cheap, GPU-friendly technique worth remembering
  if grid rendering ever becomes a performance bottleneck on large maps.
  Its measurement system supports four distance algorithms
  (chebyshev/alternating/euclidean/manhattan) as one enum per scene — worth
  noting that parfaits' two-independent-simultaneous-primitives design
  (feet + cell-count, toggleable independently) is actually more flexible
  than OBR's single-mode-per-scene approach, not a gap to close.

## Initiative & turn-order mechanics

Surveyed: PlanarAlly, FoundryVTT (`dnd5e`, `pf2e`, `crucible`), MapTool.

Concrete feature ideas, roughly in order of how self-contained they'd be to
add on top of parfaits' existing base rank + `:initiative/move` system:

- **Grouped initiative rolling** (FoundryVTT dnd5e,
  `module/documents/combat.mjs` `Combat5e#rollInitiative`): combatants
  sharing a grouping key (same actor + formula — in practice, identical
  NPCs) roll once and the rest derive their initiative from that single
  roll, instead of re-rolling for every individual monster figure. Would
  pair naturally with a monster-ability-deck system where many figures of
  one monster type share one deck's initiative value anyway (see
  Gloomhaven section below — this may end up free once ability decks
  exist, since a deck's drawn initiative already applies to every figure of
  that type).
- **Richer tiebreak chains** (pf2e `EncounterPF2e#_sortCombatants`,
  crucible `_sortCombatants`): pf2e resolves ties via an override hook →
  per-actor `tiebreakPriority` → id (always reproducible, no re-rolling).
  Crucible chains five levels (initiative → held-action "delay" flag →
  ability bonus → action points → actor type → name/id). Parfaits'
  current `(initiative/rank, :db/id)` two-level tiebreak is simpler and
  probably fine, but this is the reference if a third tiebreak level is
  ever needed.
- **"Delay" / held-action mechanic** (crucible `_sortCombatants`,
  `_onStartRound`): a combatant can hold their action and reinsert later
  at a target initiative value; originally-faster delayers go first among
  delayers. Not a small feature, but notable if turn-order ever needs to
  support "hold my turn."
- **Timed-effect duration counters + camera-follows-active-turn**
  (PlanarAlly's initiative tracker, per `planarally-docs`): initiative
  entries can carry auto-decrementing/expiring duration counters, and the
  camera can auto-lock to whichever token's turn is active. Two
  independent, fairly small UI/UX additions if wanted later.
- **Round-boundary batching pattern** (MapTool `InitiativeList.java`): a
  `holdUpdate` counter suppresses redundant change events during bulk
  edits, and `next()`/`previous()` correctly roll the round counter
  forward/back at list boundaries — useful reference if parfaits' own
  round-boundary logic ever has edge-case bugs to debug against.

## Image / token handling

Surveyed: RPTools TokenTool (`RPTools/TokenTool`).

- TokenTool turned out to be a shape-overlay **compositor**, not a
  calibrated-anchor cropper: it layers a pre-made PNG mask (folders
  literally named `Square`/`Hex`/`Round`/`Cards`) over a source image, with
  only width/height/aspect-lock controls — no anchor-point concept at all.
  **Parfaits' `:image/anchor` calibration + server-side crop route
  (`ogres.server.core/handle-thumbnail`) is already more sophisticated in
  this dimension** than the closest reference tool surveyed. The one idea
  worth borrowing: offering shaped-mask presets (hex/round border overlays)
  as a cosmetic option, rather than committing every token thumbnail to a
  plain square crop — low priority, purely cosmetic.

## Gloomhaven-specific mechanics

Surveyed: `gloom`, `gloomycompanion`, `gloomhaven-deck`, `haven-keeper`,
`gloomhavensecretary` / `Lurkars/gloomhavensecretariat`,
`Lurkars/frosthaven-previouslyon`, `Lurkars/ghs-server`, `GloomhavenHelper`
(binary only, nothing to inspect), `gloomhaven-full-stack` (README only, no
source ever committed — nothing to inspect).

### Monster ability decks (the big one — see priority #1 above)

`gloomycompanion` (`gloomycompanion/cards.js`, `monster_stats.js`,
`logic.js`) has the cleanest, most portable model found in the whole
survey:

- **Deck definition** (static, per monster *class* — many monster types
  share one deck): an ordered array of exactly 8 card tuples,
  `[shuffle_next: bool, initiative: "NN" string, ...effect_lines]`, where
  effect lines are markup strings with token placeholders (`%attack%`,
  `%move%`, `%range%`, `%push%`, `%immobilize%`, `%fire%…%use_element%`).
  `DECKS` maps monster-type name → deck class.
- **Deck runtime state** (`logic.js`'s `load_ability_deck`): a `deck`
  object with `draw_pile`/`discard` arrays of card objects
  (`{id, ui, shuffle_next, initiative, starting_lines}`), plus
  `draw_top_card`, `must_reshuffle` (checks whether the *last-discarded*
  card had `shuffle_next: true` — the reshuffle icon triggers reshuffle
  on the *next* draw, not immediately), and `reshuffle`/`shuffle_deck`.
- **Monster level stats are a separate table** (`monster_stats.js`):
  `MONSTER_STATS.monsters[name].level[N].{normal,elite}.{health,move,attack,range,attributes}`,
  merged with a drawn card's deltas (e.g. `%attack% +1`) at render time —
  decoupled from the deck itself.
  Bosses use a variant `special1`/`special2` template plus
  `immunities`/`notes`.
- **Scenario → deck mapping** (`scenarios.js`): `SCENARIO_DEFINITIONS`
  lists which monster decks are in play per scenario, with
  `SPECIAL_RULES` overrides (e.g. a monster reappearing at +2 levels).
- Scope decision for later: whether to model full card effect text (icon
  parsing, elemental infusion) or just initiative + an opaque flavor-text
  string (much simpler, still gives correct turn order). One deck instance
  is shared by every figure of that monster type in a scenario — RAW, and
  already how gloomycompanion models it.
- `gloomhaven-deck` (`yet-an-other-gloomhaven-companion/js/abilities.js`)
  is a *different* mechanic — player-hand management (cards in
  hand/played/discarded/destroyed/on-board, two-selected-per-round,
  long-rest logic) plus loot/battle-goals/enhancements tracking. Useful
  reference only if parfaits ever wants the player-side ability-hand
  equivalent; not needed for monster decks specifically.
- `gloom` (`gloom/js/`) is actually a hex battle-map/token board, not an
  ability-deck tool despite the name — its `FIGURE_HIGHLIGHT` enum
  (`MONSTER_FOCUS`/`CHARACTER_FOCUS`/etc.) is a manual annotation toggle
  for "who is this monster targeting," not real AI — not useful for the
  deck mechanic, only marginally relevant for hex/AOE-template/line-of-sight
  ideas if that's ever revisited.

### Monster stat-by-level table (see priority #2 above)

`haven-keeper` (`haven-keeper/src/app/models/monster-stat-card.ts`,
backed by `src/assets/data/monsters.json`) has the cleanest standalone
version of this, decoupled from any deck/UI concerns:

```
MonsterBase.levels: {[level 0-7]: {hitPoints, movement, attack, range?, bonuses?, attackEffects?, immunities?}}
+ eliteLevels (same shape) + abilityDeckKey
Boss variant: specialLevels: {1: MonsterAbility[], 2: MonsterAbility[]} (top/bottom action rows)
```

Per-figure/token state (`haven-keeper/src/app/models/monster-set.ts`):
`MonsterStandee = {id, rank: normal|elite|boss, hitPoints, conditions: ConditionKey[]}`.

`gloomhavensecretariat` (`Lurkars/gloomhavensecretariat/src/app/game/model/data/*.ts`)
has a heavier, edition-aware equivalent (`Monster.ts` merges a
`baseStat` fallback into sparse per-level entries) — worth a look only if
haven-keeper's fully-enumerated table turns out to be too repetitive to
maintain by hand; otherwise haven-keeper's shape is simpler and sufficient.

### Condition badges (see priority #3 above)

`gloomhavensecretariat`'s `Condition.ts` has a `ConditionName` enum of
~25 conditions, each auto-tagged with a classification (entity/standard/
character/monster/stack/stackable/turn/afterTurn/expire/positive/
negative/hidden). Full classification is overkill for a VTT that isn't
automating turn resolution, but the ~15 core icons (wound, poison,
immobilize, stun, muddle, disarm, strengthen, bless, curse, invisible,
ward, regenerate, brittle, bane, impair) would slot directly into
parfaits' existing generalized `:token-badge` vocabulary
(`src/main/ogres/app/component/scene.cljs`) as a small, self-contained
addition — no new mechanism required, just new data plus maybe two
`:expires-on-turn?`/`:stacks?` metadata flags per badge if that level of
behavior is wanted later.

### Scenario/campaign unlock tracking (lower priority, bigger lift)

`gloomhavensecretariat`'s `ScenarioData.ts` models scenario unlocks as
`requires: string[][]` (AND-of-OR dependencies) plus a richer
`ScenarioRequirement[]` (achievements, buildings, campaign stickers,
character-specific chains). `Lurkars/frosthaven-previouslyon` is a much
simpler predecessor-linked-list-with-recap-text model, closer to a content
authoring format than a generalizable data structure. Neither is a small
addition — flagged here for awareness, not recommended as a near-term
next step; scenario/campaign tracking is a genuinely different kind of
feature (persistent campaign state across sessions) than anything parfaits
currently models.

`Lurkars/ghs-server`'s backend (Java/Spring, `db/postgresql/create.sql`)
is worth one structural note: it's not a normalized relational schema at
all — three tables (`games`, `game_codes`, `settings`) holding opaque
JSON blobs keyed by a shareable game code. Structurally closer to
parfaits' own client-heavy DataScript approach than to a "real" backend;
nothing to borrow schema-wise.

## Source directory index

For re-exploring later. All paths relative to `~/Documents/Inspirations/`.

| Directory | What it is |
|---|---|
| `Fari/fari-app`, `fari-peer-server`, `fari-community`, `keeper` | Narrative VTT (Fate/PbtA-style); peer-server is now dead/unused |
| `FoundryVTT/{dnd5e,pf2e,crucible,black-flag,worldbuilding,hexploration,dungeon-tilesets,Ferncombe,foundryvtt-cli,unfulfilled-rolls}` | FoundryVTT ruleset/module implementations (Foundry core itself not included, closed-source) |
| `OwlbearRodeo/owlbear-rodeo-legacy` | Original Owlbear Rodeo core app (pre-rewrite) |
| `OwlbearRodeo/{sdk,sdk-tutorials,initiative-tracker,dynamic-fog,ranges,colored-rings,weather,prefabs,outliner,page-icon,kenku-fm}` | Modern OBR SDK + extensions |
| `PlanarAlly/{PlanarAlly,planarally-dice,planarally-docs}` | Full open-source VTT with authoritative Python/SQL server |
| `Roll20/{roll20-character-sheets,roll20-api-scripts,roll20-beacon-sheets,beacon-docs}` | Character sheet templates (legacy + modern "Beacon") and GM automation scripts |
| `RPTools/{maptool,TokenTool,maptool-server-registry,dice,dicelib,dicetool,advanced-dice-roller,parser,...}` | MapTool VTT (Java, mature), token-art compositor, dice libs |
| `gloom` | Gloomhaven hex battle-map/token board (not an ability-deck tool) |
| `gloomycompanion` | Monster ability-deck manager — **the** reference for that mechanic |
| `gloomhaven-deck` | Player ability-hand + loot/battle-goals manager |
| `haven-keeper` | Scenario bookkeeper — **the** reference for monster stat-by-level tables |
| `gloomhavensecretary`, `Lurkars/gloomhavensecretariat` | Full digital companion (scenario unlocks, condition classification, monster entities) |
| `Lurkars/frosthaven-previouslyon` | "Where did we leave off" story-resume tool |
| `Lurkars/ghs-server` | Backend for gloomhavensecretariat (JSON-blob-over-Postgres) |
| `GloomhavenHelper` | Binary only, no source — nothing inspectable |
| `gloomhaven-full-stack` | README + screenshots only, no source ever committed |
| `images` | weserv/images (C++/libvips resize proxy) — already mined for parfaits' self-hosted `/thumbnail` route, see `src/main/ogres/server/core.clj` |
