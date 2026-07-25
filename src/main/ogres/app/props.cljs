(ns ogres.app.props
  "Pure logic for the physical, on-canvas prop-copy and pile mechanism --
   mirrors the role ogres.app.cards plays for the abstract card/deck
   system. No DataScript, no UI. See events.cljs's :props/* methods and
   component/panel_props.cljs/scene_context_menu.cljs.

   A 'pile' has no dedicated entity -- it's an emergent grouping of prop
   entities that happen to share a :pile/id inside their generic
   :object/variables map, ordered by a :pile/position ordinal within
   that map. This mirrors how the abstract deck system's :card/location/
   :card/position are themselves plain, unenforced keywords/longs with
   no schema entries -- a pile here is discovered by filtering
   :scene/props, exactly how :deck/cards are filtered by
   :card/location.")

(defn grid-offset
  "The [dx dy] scene-space offset of the `idx`-th cell in a row-major
   grid `columns` wide, `spacing` scene-units apart -- cell 0 is the
   origin."
  [idx columns spacing]
  [(* spacing (mod idx columns)) (* spacing (quot idx columns))])

(defn in-pile?
  "True if the given entity is a member of the pile identified by
   `pile-id` -- i.e. its :object/variables map's :pile/id matches."
  [pile-id entity]
  (= (:pile/id (:object/variables entity)) pile-id))

(defn pile
  "The subset of `props` that are members of the pile identified by
   `pile-id`."
  [props pile-id]
  (filter (partial in-pile? pile-id) props))

(defn top-of-pile
  "The member of `props` (already filtered to one pile, see `pile`) with
   the highest :pile/position, or nil if `props` is empty -- the same
   'top of pile = max position' idiom cards.cljs/events.cljs already use
   for the abstract deck system, just reading position from a
   :object/variables map key instead of a dedicated :card/position
   attribute."
  [props]
  (if (seq props)
    (apply max-key (comp :pile/position :object/variables) props)))

(defn next-pile-position
  "One past the highest :pile/position among `props` (already filtered
   to one pile), or 0 if `props` is empty -- the position a newly
   stacked/discarded member should take to land on top."
  [props]
  (if (seq props)
    (inc (apply max (map (comp :pile/position :object/variables) props)))
    0))
