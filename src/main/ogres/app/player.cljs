(ns ogres.app.player
  "Pure data/logic for the player/NPC roster -- the color palettes and the
   'pick the first not-already-taken color' assignment rule. No
   DataScript, no UI; mirrors the role ogres.app.cards plays for the
   card/deck system. See events.cljs's :player/* methods and
   component/panel_roster.cljs.

   Humans and NPCs draw from two disjoint palettes -- human colors are
   bright/high-visibility, NPC colors are darker/muted, so the two kinds
   are visually distinguishable from one another at a glance in addition
   to each participant being distinguishable within their own kind.
   Because the two palettes never share a key, colors never need to be
   reserved across kinds -- only within one.")

(def human-colors
  "The human roster palette, in display order -- [key label] pairs. `key`
   is both the value stored in :player/color and the CSS [data-color]
   this app's existing color system already resolves (see
   resource/color.css); `label` is what the UI shows."
  [["red" "Red"] ["blue" "Blue"] ["teal" "Teal"] ["purple" "Purple"]
   ["yellow" "Yellow"] ["orange" "Orange"] ["green" "Green"] ["pink" "Pink"]
   ["gray" "Grey"] ["sky" "Light Blue"] ["dark-green" "Dark Green"]
   ["brown" "Brown"]])

(def npc-colors
  "The NPC roster palette -- darker/muted shades of the same hue families
   as `human-colors`, so NPCs read as visually distinct from players
   without needing all-new hues. Same [key label] shape as
   `human-colors`; keys are namespaced with an 'npc-' prefix so they can
   never collide with a human color key."
  [["npc-red" "Maroon"] ["npc-blue" "Navy"] ["npc-teal" "Deep Teal"]
   ["npc-purple" "Plum"] ["npc-yellow" "Olive"] ["npc-orange" "Rust"]
   ["npc-green" "Forest"] ["npc-pink" "Mauve"] ["npc-gray" "Charcoal"]
   ["npc-sky" "Steel Blue"] ["npc-slate" "Slate"] ["npc-stone" "Umber"]])

(defn colors-for-kind
  "The applicable palette ([key label] pairs) for the given player kind
   (:human or :npc)."
  [kind]
  (if (= kind :npc) npc-colors human-colors))

(defn next-color
  "The first color key in `colors` (a [key label] pairs seq, see
   `colors-for-kind`) not present in `taken` (a set of already-used color
   keys), or the palette's first color if every one is taken -- degrades
   gracefully rather than blocking player creation, the same fallback
   ogres.app.provider.session/next-color already uses for connected
   users' cursor colors."
  [colors taken]
  (or (first (remove taken (map first colors)))
      (ffirst colors)))

(defn authority?
  "True if `viewer-uuid` has visibility/edit authority over an asset.
   `host?` is whether the viewer is the host. `controller-uuid` is the
   asset's assigned player's current controller (:player/controller's
   :user/uuid), or nil if unassigned. `connected-uuids` is the set of
   currently-connected :user/uuid values.

   When the asset is unassigned, or its controller has disconnected
   (stale ref), authority reverts to the host -- today's plain
   host-only behavior. When a *connected* controller is assigned,
   authority belongs to that controller ALONE -- even the host does not
   automatically see/toggle hidden state for it. This is what lets a
   controlling player hide their own assigned token or prop from the
   host, not just from other players."
  [viewer-uuid host? connected-uuids controller-uuid]
  (if (and controller-uuid (contains? connected-uuids controller-uuid))
    (= controller-uuid viewer-uuid)
    host?))
