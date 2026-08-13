(ns ogres.app.events
  (:require [datascript.core :as ds]
            [clojure.set :refer [union difference]]
            [clojure.string :refer [trim]]
            [ogres.app.attack-deck :as attack-deck]
            [ogres.app.cards :as cards]
            [ogres.app.const :refer [grid-size hex-radius]]
            [ogres.app.crazy-eights :as crazy-eights]
            [ogres.app.dice :as dice]
            [ogres.app.game-type :as game-type]
            [ogres.app.geom :as geom]
            [ogres.app.go-fish :as go-fish]
            [ogres.app.matrix :as matrix]
            [ogres.app.memory :as memory]
            [ogres.app.old-maid :as old-maid]
            [ogres.app.player :as player]
            [ogres.app.props :as props]
            [ogres.app.provider.state :as state]
            [ogres.app.rummy :as rummy]
            [ogres.app.segment :as seg]
            [ogres.app.turn-order :as turn-order]
            [ogres.app.vec :as vec :refer [Vec2]]
            [ogres.app.war :as war]))

(def ^:private suffix-max-xf
  (map (fn [[label tokens]] [label (apply max (map :initiative/suffix tokens))])))

(def ^:private zoom-scales
  [0.15 0.30 0.50 0.75 0.90 1 1.25 1.50 2 3 4])

(defn ^:private linear [dx dy rx ry]
  (fn [n] (+ (* (/ (- n dx) (- dy dx)) (- ry rx)) rx)))

(defn ^:private indexed
  "Returns a transducer which decorates each element with a decreasing
   negative index suitable for use as temporary ids in a DataScript
   transaction. Optionally receives an offset integer to begin counting and
   a step integer to create space between indexes."
  ([]
   (indexed 1 1))
  ([offset]
   (indexed offset 1))
  ([offset step]
   (map-indexed (fn [idx val] [(-> (* idx step) (+ offset) (* -1)) val]))))

(defn ^:private suffix-token-key
  "Returns a grouping key for the given token that will match other similarly
   identifiable tokens."
  [token]
  (let [{label :token/label {hash :image/hash} :token/image} token]
    [label hash]))

(defn ^:private suffixes
  "Returns a map of `{entity key => suffix}` for the given token entities.
   Each suffix represents a unique and stable identity for a token within
   the group of tokens by which it shares a label. Suffixes are intended to
   help decorate tokens that may otherwise be difficult to distinguish
   when they share the same image and label."
  [tokens]
  (let [groups (group-by suffix-token-key tokens)
        offset (into {} suffix-max-xf groups)]
    (loop [tokens tokens index {} result {}]
      (if (seq tokens)
        (let [token (first tokens)
              group (suffix-token-key token)]
          (if (or (= (count (groups group)) 1)
                  (:initiative/suffix token)
                  (contains? (:token/flags token) :player))
            (recur (rest tokens) index result)
            (recur (rest tokens)
                   (update index group inc)
                   (assoc result (:db/id token) (+ (offset group) (index group) 1)))))
        result))))

(defn ^:private to-precision [n p]
  (js/Number (.toFixed (js/Number.parseFloat n) p)))

(defn ^:private constrain [n min max]
  (clojure.core/max (clojure.core/min n max) min))

(defn ^:private initiative-order
  "Descending sort by turn-order rank, :db/id as a total-order tiebreak.
   A token with no rank yet (nil) sorts after every ranked token (cljs
   `compare` treats nil as less than any number) but still has a stable
   position relative to other unranked tokens via the id tiebreak -- so a
   scene where nobody has been assigned an order at all just sorts by
   :db/id, deterministically. Game-agnostic: this is the base turn-order
   comparator every game-type shares, regardless of *how* a rank got set
   (manually via :initiative/move, or by a game module's own mechanism,
   e.g. D&D's d20 roll -- see ogres.app.game-type.games.dnd5e).

   This is also what makes the floor case work with zero setup: two
   participants who never get a rank at all (e.g. a two-player game like
   chess, with strictly alternating fixed turns) still sort the same
   stable way every time, round after round, since neither :db/id nor
   the absence of a rank ever changes between rounds -- :initiative/next
   just keeps cycling the same fixed order. Nothing about ranking,
   rolling, or manual reordering is required to get a working, repeating
   turn order; those are all optional refinements layered on top of this
   baseline."
  [a b]
  (let [f (juxt :initiative/rank :db/id)]
    (compare (f b) (f a))))

(defmulti event-tx-fn (fn [_ event] event))

(defmethod event-tx-fn :default [] [])

;; -- Local --
(defmethod
  ^{:doc "Change the selected panel to the keyword given by `panel`."}
  event-tx-fn :user/select-panel
  [_ _ panel]
  [{:db/ident :user :panel/selected panel :panel/expanded true}])

(defmethod
  ^{:doc "Toggle the expanded state of the panel."}
  event-tx-fn :user/toggle-panel
  [data]
  (let [user (ds/entity data [:db/ident :user])]
    [{:db/ident :user :panel/expanded (not (get user :panel/expanded true))}]))

(defmethod
  ^{:doc "Switches the local user's interface mode (:builder, :setup, or
          :play). Builder mode edits game-type templates; setup mode
          constructs a scenario; play mode hides construction tools.
          Switching to setup mode always lands on the Scene tab, rather
          than leaving whatever tab happened to carry over selected from
          another mode."}
  event-tx-fn :user/change-mode
  [_ _ mode]
  [(cond-> {:db/ident :user :user/mode mode}
     (= mode :setup)
     (assoc :panel/selected :scene))])

(defmethod
  ^{:doc "Changes the character label for the given user."}
  event-tx-fn :user/change-label
  ([_ _ value]
   [{:db/ident :user :user/label value}])
  ([_ _ uuid value]
   [{:user/uuid uuid :user/label value}]))

(defmethod
  ^{:doc "Changes the character description for the given user."}
  event-tx-fn :user/change-description
  ([_ _ value]
   [{:db/ident :user :user/description value}])
  ([_ _ uuid value]
   [{:user/uuid uuid :user/description value}]))

(defmethod
  ^{:doc "Changes the character image for the given user."}
  event-tx-fn :user/change-image
  ([_ _ hash]
   [{:db/ident :user :user/image [:image/hash hash]}])
  ([_ _ uuid hash]
   [{:user/uuid uuid :user/image [:image/hash hash]}]))

(defmethod
  ^{:doc "Changes the character label and description for the given user."}
  event-tx-fn :user/change-details
  ([_ _ label description]
   [{:db/ident :user :user/label label :user/description description}])
  ([_ _ uuid label description]
   [{:user/uuid uuid :user/label label :user/description description}]))

;; -- Camera --
(defn ^:private assoc-camera
  [data & kvs]
  (let [user (ds/entity data [:db/ident :user])]
    [(apply assoc {:db/id (:db/id (:user/camera user))} kvs)]))

(defmethod
  ^{:doc "Changes the public label for the current camera."}
  event-tx-fn :camera/change-label
  [_ _ label]
  [[:db.fn/call assoc-camera :camera/label label]])

(defmethod
  ^{:doc "Removes the public label for the current camera."}
  event-tx-fn :camera/remove-label
  [data]
  (let [user (ds/entity data [:db/ident :user])]
    [[:db/retract (:db/id (:user/camera user)) :camera/label]]))

(defmethod
  ^{:doc "Translate the current camera by the screen-space offset given by
          dx and dy, accounting for camera scale and, for isometric
          grid-types, the fixed isometric projection -- without this, the
          live drag preview (a raw, unconverted screen-pixel overlay) and
          the committed camera position (which renders through the full
          iso matrix) fall out of sync, and the view visibly jumps the
          instant the drag ends."}
  event-tx-fn :camera/translate
  [data _ delta]
  (let [{{id :db/id point :camera/point scale :camera/scale
          {grid-type :scene/grid-type} :camera/scene} :user/camera}
        (ds/entity data [:db/ident :user])]
    [{:db/id id :camera/point (vec/add (or point vec/zero) (geom/screen->scene-vec delta scale grid-type))}]))

(defmethod
  ^{:doc "Changes the camera draw mode to the given value. The draw mode is
          used to decide what behavior clicking and dragging on the scene
          will have, such as drawing a shape or determining the distance
          between two points.

          Clicking a toolbar tool's own button again while it's already the
          active mode toggles back to :select instead of re-applying the
          same mode -- every toolbar action button already renders
          :aria-pressed based on whether it's the active mode (see
          toolbar.cljs), so this makes that pressed state a genuine toggle
          instead of a one-way switch that only Escape (:shortcut/escape)
          could undo."}
  event-tx-fn :camera/change-mode
  [data _ mode]
  (let [user (ds/entity data [:db/ident :user])
        current (:camera/draw-mode (:user/camera user))
        next (if (= current mode) :select mode)]
    (if (or (:user/host user) (not (#{:mask :mask-toggle :mask-remove :grid :note :object-anchor} next)))
      [{:db/id (:db/id (:user/camera user)) :camera/draw-mode next}]
      [])))

(defmethod
  ^{:doc "Changes the zoom value for the current camera by the given value
          `next` and, optionally, a cursor point given by `x` and `y`.
          This method preserves the point of the cursor on the scene,
          adjusting the camera point to ensure that the user feels as if
          they are zooming in or out from their cursor."}
  event-tx-fn :camera/zoom-change
  ([data event & args]
   (let [user (ds/entity data [:db/ident :user])]
     (case (count args)
       0 [[:db.fn/call event-tx-fn event 1]]
       1 (let [[scale] args
               bounds (or (:user/bounds user) seg/zero)]
           [[:db.fn/call event-tx-fn event scale (seg/midpoint bounds)]])
       (let [[next-scale point] args
             {{id :db/id scale :camera/scale camera :camera/point
               {grid-type :scene/grid-type} :camera/scene} :user/camera} user
             delta (-> (vec/mul point (/ next-scale (or scale 1)))
                       (vec/sub point)
                       (vec/div next-scale))]
         [{:db/id id
           :camera/scale next-scale
           :camera/point (vec/add (geom/iso-inverse grid-type delta) camera)}])))))

(defmethod
  ^{:doc "Changes the zoom value for the current camera by offsetting it from
          the given value `delta`. This is useful for zooming with a device
          that uses fine grained updates such as a mousewheel or a trackpad."}
  event-tx-fn :camera/zoom-delta
  [data _ mx my delta trackpad?]
  (let [user (ds/entity data [:db/ident :user])
        bound (or (:user/bounds user) seg/zero)
        scale (linear -400 400 -0.50 0.50)
        delta (if trackpad? (scale (* -1 8 delta)) (scale (* -1 2 delta)))
        point (vec/sub (Vec2. mx my) (.-a bound))
        scale (-> (:camera/scale (:user/camera user)) (or 1)
                  (js/Math.log) (+ delta) (js/Math.exp)
                  (to-precision 2) (constrain 0.15 4))]
    [[:db.fn/call event-tx-fn :camera/zoom-change scale point]]))

(defmethod
  ^{:doc "Increases the zoom value for the current camera to the next nearest
          zoom level. These fixed zoom levels are determined by an internal
          constant."}
  event-tx-fn :camera/zoom-in
  [data]
  (let [user   (ds/entity data [:db/ident :user])
        camera (:user/camera user)
        prev   (or (:camera/scale camera) 1)
        next   (reduce (fn [n s] (if (> s prev) (reduced s) n)) prev zoom-scales)]
    [[:db.fn/call event-tx-fn :camera/zoom-change next]]))

(defmethod
  ^{:doc "Decreases the zoom value for the current camera to the nearest
          previous zoom level. These fixed zoom levels are determined by an
          internal constant."}
  event-tx-fn :camera/zoom-out
  [data]
  (let [user   (ds/entity data [:db/ident :user])
        camera (:user/camera user)
        prev   (or (:camera/scale camera) 1)
        next   (reduce (fn [n s] (if (< s prev) (reduced s) n)) prev (reverse zoom-scales))]
    [[:db.fn/call event-tx-fn :camera/zoom-change next]]))

(defmethod
  ^{:doc "Resets the zoom value for the given camera to its default setting of
          100%."}
  event-tx-fn :camera/zoom-reset
  []
  [[:db.fn/call event-tx-fn :camera/zoom-change 1]])

;; -- Scenes --
(defmethod
  ^{:doc "Creates a new blank scene and corresponding camera for the local user
          then switches them to it. Also resets :user/game-type-editing to
          the new scene's own (Default) game-type -- game types are
          isolated to Game Builder mode (see :user/edit-game-type's own
          doc), so without this, opening Builder mode right after
          creating a scene would misleadingly show whatever template was
          last being edited as still 'selected', even though this brand
          new scene actually starts on Default."}
  event-tx-fn :scenes/create
  []
  [[:db/add -1 :db/ident :root]
   [:db/add -1 :root/scenes -2]
   [:db/add -2 :db/empty true]
   [:db/add -2 :scene/game-type [:game-type/key :default]]
   [:db/add -1 :root/user -3]
   [:db/add -3 :db/ident :user]
   [:db/add -3 :user/camera -4]
   [:db/add -3 :user/cameras -4]
   [:db/add -3 :user/game-type-editing [:game-type/key :default]]
   [:db/add -4 :camera/scene -2]
   [:db/add -4 :camera/point vec/zero]])

(defmethod
  ^{:doc "Switches to the given scene by the given camera identifier.
          Also resyncs :user/game-type-editing to that scene's own
          :scene/game-type -- without this, Game Builder mode (if opened
          after switching) would keep showing whichever template was
          last edited rather than the newly active scene's actual one,
          the same staleness :scenes/create's own doc explains."}
  event-tx-fn :scenes/change
  [data _ id]
  (let [game-type-id (:db/id (:scene/game-type (:camera/scene (ds/entity data id))))]
    [{:db/ident :user :user/camera id :user/game-type-editing game-type-id}]))

(defmethod
  ^{:doc "Removes the scene and corresponding camera for the local user. Also
          removes all scene cameras for any connected users and switches them
          to whichever scene the host is now on. Whenever the local user's
          own camera moves to a different scene as a result (any of the
          three branches below), :user/game-type-editing is resynced to
          that scene's own :scene/game-type -- the same staleness
          :scenes/create's and :scenes/change's own docs explain."}
  event-tx-fn :scenes/remove
  [data _ camera-id]
  (let [root (ds/entity data [:db/ident :root])
        user (ds/entity data [:db/ident :user])
        prev-cam (ds/entity data camera-id)
        prev-scn (:db/id (:camera/scene prev-cam))]
    (conj
     (if (= (:db/id (:user/camera user)) (:db/id prev-cam))
       (if-let [next-scn (:db/id (first (remove (comp #{prev-scn} :db/id) (:root/scenes root))))]
         (let [game-type-id (:db/id (:scene/game-type (ds/entity data next-scn)))]
           (if-let [next-cam (:db/id (first (filter (comp #{next-scn} :db/id :camera/scene) (:user/cameras user))))]
             [[:db/add (:db/id user) :user/camera next-cam]
              [:db/add (:db/id user) :user/game-type-editing game-type-id]]
             [[:db/add (:db/id user) :user/camera -1]
              [:db/add (:db/id user) :user/cameras -1]
              [:db/add (:db/id user) :user/game-type-editing game-type-id]
              [:db/add -1 :camera/scene next-scn]
              [:db/add -1 :camera/point vec/zero]]))
         [[:db/add (:db/id root) :root/scenes -2]
          [:db/add (:db/id user) :user/camera -1]
          [:db/add (:db/id user) :user/cameras -1]
          [:db/add (:db/id user) :user/game-type-editing [:game-type/key :default]]
          [:db/add -1 :camera/scene -2]
          [:db/add -1 :camera/point vec/zero]
          [:db/add -2 :db/empty true]
          [:db/add -2 :scene/game-type [:game-type/key :default]]]) [])
     [:db.fn/call event-tx-fn :scenes/sync-with-user prev-scn]
     [:db/retractEntity prev-scn]
     [:db/retractEntity camera-id])))

(defmethod
  ^{:doc "Find all players that are currently viewing the given scene and
          move them to the scene being viewd by the current user."}
  event-tx-fn
  :scenes/sync-with-user
  [data _ prev-scn]
  (let [next-scn (-> (ds/entity data [:db/ident :user]) :user/camera :camera/scene :db/id)]
    (->> (for [conn (:session/conns (ds/entity data [:db/ident :session]))
               :let [curr-scn (-> conn :user/camera :camera/scene :db/id)]
               :when (= curr-scn prev-scn)
               :let [curr-cam (first (filter (comp #{curr-scn} :db/id :camera/scene) (:user/cameras conn)))]]
           (if-let [next-cam (first (filter (comp #{next-scn} :db/id :camera/scene) (:user/cameras conn)))]
             [[:db/retractEntity (:db/id curr-cam)]
              [:db/add (:db/id conn) :user/camera (:db/id next-cam)]]
             [[:db/retractEntity (:db/id curr-cam)]
              [:db/add (:db/id conn) :user/cameras -3]
              [:db/add (:db/id conn) :user/camera -3]
              [:db/add -3 :camera/scene next-scn]
              [:db/add -3 :camera/point vec/zero]]))
         (apply concat))))

;; -- Game Types --
(defmethod
  ^{:doc "Creates a new game-type template, cloning the given source
          game-type's enabled elements and icon overrides so it starts
          non-empty, then opens it for editing in Builder mode."}
  event-tx-fn :game-type/create
  [data _ source-id name]
  (let [source (if source-id (ds/entity data source-id))]
    [{:db/id -1
      :game-type/name name
      :game-type/enabled-elements (into #{} (:game-type/enabled-elements source))
      :game-type/icon-overrides (into {} (:game-type/icon-overrides source))}
     [:db/add [:db/ident :root] :root/game-types -1]
     [:db.fn/call event-tx-fn :user/edit-game-type -1]]))

(defmethod
  ^{:doc "Renames the given game-type template."}
  event-tx-fn :game-type/rename
  [_ _ game-type-id name]
  [{:db/id game-type-id :game-type/name (trim name)}])

(defmethod
  ^{:doc "Sets how much real-world distance one grid cell represents for
          this game-type, and what that distance is called -- 5 / \"ft.\"
          is D&D's convention and the default.

          Presentation only. The grid stays `grid-size` px per cell and
          :token/size, :token/light and :token/aura-radius stay in fixed
          scene units; this changes what the measurement tool PRINTS and
          nothing else, so a game can measure in metres, hexes, miles or
          parsecs without any of the geometry moving. See component/
          scene-draw's px->distance. A game-type that measures in bare
          cells wants :tool/measurement-cells instead and can ignore
          this entirely."}
  event-tx-fn :game-type/change-distance
  [_ _ game-type-id per-cell unit]
  (into (if (and (number? per-cell) (pos? per-cell))
          [[:db/add game-type-id :game-type/distance-per-cell per-cell]]
          [[:db/retract game-type-id :game-type/distance-per-cell]])
        (if (and (string? unit) (seq (trim unit)))
          [[:db/add game-type-id :game-type/distance-unit (trim unit)]]
          [[:db/retract game-type-id :game-type/distance-unit]])))

(defmethod
  ^{:doc "Removes the given game-type template. Refuses to remove the
          last remaining template -- a scene must always have one to
          reference. Any scene using it, and the Builder mode 'currently
          editing' selection, fall back to another remaining template
          (the bundled Default, if present, otherwise whichever is
          first)."}
  event-tx-fn :game-type/remove
  [data _ game-type-id]
  (let [root (ds/entity data [:db/ident :root])
        remaining (remove (comp #{game-type-id} :db/id) (:root/game-types root))]
    (if (empty? remaining)
      []
      (let [fallback (:db/id (or (first (filter (comp #{:default} :game-type/key) remaining))
                                  (first remaining)))
            editing (:db/id (:user/game-type-editing (ds/entity data [:db/ident :user])))]
        (into [[:db/retractEntity game-type-id]]
              (concat
               (for [scene (:scene/_game-type (ds/entity data game-type-id))]
                 {:db/id (:db/id scene) :scene/game-type fallback})
               (when (= editing game-type-id)
                 [{:db/ident :user :user/game-type-editing fallback}])))))))

(defmethod
  ^{:doc "Imports a game-type template from data previously produced by
          exporting one (see the Game Builder panel), creating a new
          template and opening it for editing -- same as
          :game-type/create, except the initial name/elements/overrides
          come from the imported data rather than being cloned live.
          Since this data may originate from an arbitrary file, elements
          and icon overrides are sanitized down to recognized registry
          ids/shapes regardless of what the caller already did."}
  event-tx-fn :game-type/import
  [_ _ {:keys [name enabled-elements icon-overrides]}]
  [{:db/id -1
    :game-type/name (str name)
    :game-type/enabled-elements (game-type/sanitize-enabled-elements enabled-elements)
    :game-type/icon-overrides (game-type/sanitize-icon-overrides icon-overrides)}
   [:db/add [:db/ident :root] :root/game-types -1]
   [:db.fn/call event-tx-fn :user/edit-game-type -1]])

(defn ^:private enable-one
  "Adds `element-id` to `enabled` -- if it declares an :exclusive-group
   (see `ogres.app.game-type/exclusive-group`), every other currently-
   enabled member of that group is evicted in the same step, so the
   result never has two competing members of one group. The shared core
   of both :game-type/toggle-element and :game-type/toggle-category
   below; disabling has no equivalent conflict to resolve, so it's just
   a plain `disj` at each call site."
  [enabled element-id]
  (let [group (game-type/exclusive-group element-id)]
    (cond-> (conj enabled element-id)
      group (as-> s (into #{}
                           (remove #(and (not= % element-id)
                                         (= (game-type/exclusive-group %) group)))
                           s)))))

(defn ^:private grid-switch-tx
  "If `next-enabled` leaves any scene on `entity` using a grid layout
   that's no longer enabled, switches that scene to another available
   one. Shared tail of :game-type/toggle-element and
   :game-type/toggle-category -- see the former's docstring for why
   'no-grid mode' is deliberately not handled here by forcing
   :scene/show-grid/:scene/grid-align."
  [entity next-enabled]
  (when (pos? (game-type/grid-count next-enabled))
    (for [scene (:scene/_game-type entity)
          :when (not (contains? next-enabled (game-type/grid-tool-id (:scene/grid-type scene :square))))]
      {:db/id (:db/id scene) :scene/grid-type (game-type/pick-grid-type next-enabled)})))

(defmethod
  ^{:doc "Enables or disables one element (by its namespaced registry id,
          e.g. :unit/light or :tool/grid-hex-pointy) on the given
          game-type. If this leaves any scene using it on a grid layout
          that's no longer enabled, that scene is switched to another
          available layout.

          If the element being enabled declares an :exclusive-group (see
          `ogres.app.game-type/exclusive-group`), every other currently-
          enabled element sharing that group is disabled in the same
          transaction -- e.g. enabling D&D 5e's HP tracker automatically
          disables Gloomhaven's, since a single game-type should never
          end up with two competing 'the' HP trackers. Disabling an
          element never has this side effect, only enabling one does.

          'No-grid mode' (a game-type with zero grid layouts enabled)
          isn't handled here by forcing :scene/show-grid or :scene/grid-
          align -- that would need to be undone again the moment a grid
          layout became available, which is exactly the bug this
          replaced. Instead, rendering and snapping both treat 'the
          active game-type has zero grid layouts enabled' as an
          unconditional override (see scene.cljs's `has-grid?` and the
          `align?` sites in this file), independent of a scene's own
          stored show-grid/grid-align preference. That preference is
          therefore never destructively overwritten, so it's simply
          already correct again the instant a grid layout is re-enabled."}
  event-tx-fn :game-type/toggle-element
  [data _ game-type-id element-id enabled?]
  (let [entity (ds/entity data game-type-id)
        current (set (:game-type/enabled-elements entity))
        next-enabled (if enabled?
                       (enable-one current element-id)
                       (disj current element-id))]
    (into [{:db/id game-type-id :game-type/enabled-elements next-enabled}]
          (grid-switch-tx entity next-enabled))))

(defmethod
  ^{:doc "Enables or disables every one of `element-ids` on the given
          game-type at once, in a single transaction -- the 'select all'
          toggle for an entire Builder category (see
          panel_game_type_builder.cljs's `category-summary`, e.g. 'all
          D&D 5e features' or 'all Gloomhaven features'). Applies the
          same per-id :exclusive-group conflict resolution
          :game-type/toggle-element does, and the same grid-layout
          scene-switch tail -- this is exactly N individual toggles
          batched into one transaction, not a different rule.

          The optional 5-arg form additionally force-disables every id
          in `disable-ids`, in the same transaction, but only when
          `enabled?` is true -- for a module whose real-world game is
          tied to one specific map/grid type (Gloomhaven's board is
          always point-top hexagons, see
          `ogres.app.game-type/category-grid-elements`), so checking
          'all Gloomhaven features' both turns on hex-pointy and turns
          off whatever other grid layouts happened to be enabled,
          instead of just adding hex-pointy alongside them. Disabling a
          category never force-disables anything beyond `element-ids`
          itself -- there's nothing to exclude when turning things off."}
  event-tx-fn :game-type/toggle-category
  ([data event game-type-id element-ids enabled?]
   [[:db.fn/call event-tx-fn event game-type-id element-ids enabled? #{}]])
  ([data _ game-type-id element-ids enabled? disable-ids]
   (let [entity (ds/entity data game-type-id)
         current (set (:game-type/enabled-elements entity))
         next-enabled (if enabled?
                        (apply disj (reduce enable-one current element-ids) disable-ids)
                        (apply disj current element-ids))]
     (into [{:db/id game-type-id :game-type/enabled-elements next-enabled}]
           (grid-switch-tx entity next-enabled)))))

(defmethod
  ^{:doc "Sets or clears a flavor-asset icon override for one element on
          the given game-type. `link` is nil to clear the override (falling
          back to the registry's default icon), or an :icon/link map, e.g.
          {:icon/sprite-name \"…\"} or {:icon/url \"…\"}."}
  event-tx-fn :game-type/set-icon-override
  [data _ game-type-id element-id link]
  (let [current (:game-type/icon-overrides (ds/entity data game-type-id))]
    [{:db/id game-type-id
      :game-type/icon-overrides (if (some? link)
                                   (assoc (into {} current) element-id link)
                                   (dissoc (into {} current) element-id))}]))

;; -- Scene Images --
(defmethod event-tx-fn :scene-images/create-many
  [_ _ images]
  (into [{:db/ident :root
          :root/scene-images
          (for [[{:keys [hash name size width height]} _] images]
            {:image/hash hash
             :image/name name
             :image/size size
             :image/width width
             :image/height height})}] cat
        (for [[image thumbnail] images]
          (if (= (:hash image) (:hash thumbnail))
            [{:image/hash (:hash image) :image/thumbnail [:image/hash (:hash image)]}]
            [{:image/hash (:hash thumbnail)
              :image/name (:name thumbnail)
              :image/size (:size thumbnail)
              :image/width (:width thumbnail)
              :image/height (:height thumbnail)}
             {:image/hash (:hash image) :image/thumbnail [:image/hash (:hash thumbnail)]}]))))

(defmethod
  ^{:doc "Removes the scene image by the given identifying hash, along with
          any board pieces in any scene that reference it -- otherwise
          they'd be left with a dangling :board/image ref."}
  event-tx-fn :scene-images/remove
  [data _ image thumb]
  (let [root (ds/entity data [:db/ident :root])
        pieces (for [scene (:root/scenes root)
                     piece (:scene/board scene)
                     :when (= image (:image/hash (:board/image piece)))]
                 [:db/retractEntity (:db/id piece)])]
    (into (if (= image thumb)
            [[:db/retractEntity [:image/hash image]]]
            [[:db/retractEntity [:image/hash image]]
             [:db/retractEntity [:image/hash thumb]]])
          pieces)))

;; -- Scene --
(defn ^:private assoc-scene
  [data & kvs]
  (let [user (ds/entity data [:db/ident :user])
        scene (:db/id (:camera/scene (:user/camera user)))]
    [(apply assoc {:db/id scene} kvs)]))


(defmethod
  ^{:doc "Updates the grid size for the current scene. Written and read
          back only by the grid-alignment tool (component/scene-draw's
          draw-grid), which uses the previous value to space its preview
          lattice and to scale the origin it computes. Nothing renders
          from it -- the scene grid itself is always the `grid-size`
          constant -- so this is alignment-tool state, not a scene
          dimension. To change how big an IMAGE lands relative to the
          grid, see :root/change-default-cell-px and image-cell-scale."}
  event-tx-fn :scene/change-grid-size
  [_ _ size]
  [[:db.fn/call assoc-scene :scene/grid-size size]])

(defmethod
  ^{:doc "Sets the campaign-wide baseline pixel density for prop and board
          images -- how many source pixels make up one grid cell -- or
          clears it when `value` isn't a positive number.

          Hosts overwhelmingly prepare a game from one asset set produced
          at a single density, so calibrating every image by hand (drag a
          copy to size, 'Save scale as default') is repetitive setup.
          This is the fallback those per-image calibrations override; see
          image-cell-scale for the full resolution order. Applies only to
          images placed AFTER it's set -- it is a default for new
          placements, not a retroactive rescale of a built scene.

          Host-only: it is campaign-wide setup, the same concern the
          Scene panel that hosts it is already gated on."}
  event-tx-fn :root/change-default-cell-px
  [data _ value]
  (if (:user/host (ds/entity data [:db/ident :user]))
    (if (and (number? value) (pos? value))
      [[:db/add [:db/ident :root] :root/default-cell-px value]]
      [[:db/retract [:db/ident :root] :root/default-cell-px]])
    []))

(defmethod
  ^{:doc "Updates the grid type (:square, :hex-pointy, or :hex-flat) for
          the current scene."}
  event-tx-fn :scene/change-grid-type
  [_ _ type]
  [[:db.fn/call assoc-scene :scene/grid-type type]])

(defmethod
  ^{:doc "Updates the grid rendering style (:line or :dot) for the
          current scene."}
  event-tx-fn :scene/change-grid-shape
  [_ _ shape]
  [[:db.fn/call assoc-scene :scene/grid-shape shape]])

(defmethod
  ^{:doc "Updates whether the grid renders :over or :under the board layer
          (the scene's placed board/map pieces) for the current scene."}
  event-tx-fn :scene/change-grid-order-board
  [_ _ value]
  [[:db.fn/call assoc-scene :scene/grid-order-board value]])

(defmethod
  ^{:doc "Updates whether the grid renders :over or :under the props layer
          for the current scene."}
  event-tx-fn :scene/change-grid-order-props
  [_ _ value]
  [[:db.fn/call assoc-scene :scene/grid-order-props value]])

(defmethod
  ^{:doc "Updates the token scale multiplier for the given grid-type on the
          current scene. Each grid-type remembers its own token scale
          independently (stored as a sparse map keyed by grid-type) so a
          host can tune token size to fit their art on one grid type,
          switch to another to compare, and come back to find their first
          choice untouched."}
  event-tx-fn :scene/change-token-scale
  [data _ grid-type value]
  (let [scene (:camera/scene (:user/camera (ds/entity data [:db/ident :user])))
        next  (assoc (:scene/token-scale scene {}) grid-type value)]
    [[:db.fn/call assoc-scene :scene/token-scale next]]))

(defmethod
  ^{:doc "Opens the given game-type entity for editing in Game Builder mode
          and, in the same transaction, makes it the active game-type for
          the current scene -- game types are isolated to Game Builder
          mode, so selecting or creating a template there is the only way
          to change which framework elements (per-unit fields, tools,
          systems) are available while setting up or playing this scene."}
  event-tx-fn :user/edit-game-type
  [_ _ game-type-id]
  [{:db/ident :user :user/game-type-editing game-type-id}
   [:db.fn/call assoc-scene :scene/game-type game-type-id]])

(defmethod
  ^{:doc "Applies both a grid origin and tile size to the current scene."}
  event-tx-fn :scene/apply-grid-options
  [data _ origin size]
  (let [{{camera-id :db/id point :camera/point
          {scene-id :db/id prev-origin :scene/grid-origin}
          :camera/scene} :user/camera}
        (ds/entity data [:db/ident :user])]
    [{:db/id camera-id
      :camera/draw-mode :select
      :camera/point (vec/sub (vec/add point (or prev-origin vec/zero)) origin)
      :camera/scene
      {:db/id scene-id
       :scene/grid-size size
       :scene/grid-origin origin}}]))

(defmethod
  ^{:doc "Resets the grid origin to (0, 0)."}
  event-tx-fn :scene/reset-grid-origin
  [data]
  (let [user (ds/entity data [:db/ident :user])
        scene (:db/id (:camera/scene (:user/camera user)))]
    [[:db.fn/call assoc-camera :camera/draw-mode :select]
     [:db/retract scene :scene/grid-origin]]))

(defmethod
  ^{:doc "Retracts the grid size for the current scene, allowing queries to
          revert to their defaults."}
  event-tx-fn :scene/retract-grid-size
  [data]
  (let [user (ds/entity data [:db/ident :user])
        scene (:db/id (:camera/scene (:user/camera user)))]
    [[:db/retract scene :scene/grid-size]]))

(defmethod
  ^{:doc "Updates whether or not the grid is drawn onto the current scene."}
  event-tx-fn :scene/toggle-show-grid
  [_ _ value]
  [[:db.fn/call assoc-scene :scene/show-grid value]])

(defmethod
  ^{:doc "Updates whether or not dark mode is enabled on the current scene."}
  event-tx-fn :scene/toggle-dark-mode
  [_ _ enabled]
  [[:db.fn/call assoc-scene :scene/dark-mode enabled]])

(defmethod
  ^{:doc "Updates whether the current scene is in 'neutral authority'
          (impartial dealer) mode -- when true, the host no longer gets
          automatic default authority over unowned/unassigned hidden
          objects (see authorized-to-hide?'s doc); only explicit
          :object/owner/:player/controller assignments or
          :object/shared? still grant it. False (the default, matching
          every scene created before this existed) is today's ordinary
          host-omniscient behavior, appropriate for a GM running a game
          like D&D. :memory/start turns this on automatically; this
          event is the general-purpose manual toggle for any other
          scenario (Scene panel, host-only)."}
  event-tx-fn :scene/toggle-neutral-authority
  [_ _ enabled]
  [[:db.fn/call assoc-scene :scene/neutral-authority? enabled]])

(defmethod
  ^{:doc "Updates whether or not align to grid is enabled on the current scene."}
  event-tx-fn :scene/toggle-grid-align
  [_ _ enabled]
  [[:db.fn/call assoc-scene :scene/grid-align enabled]])

(defmethod
  ^{:doc "Updates whether or not object outlines are drawn on the current scene."}
  event-tx-fn :scene/toggle-object-outlines
  [_ _ enabled]
  [[:db.fn/call assoc-scene :scene/show-object-outlines enabled]])

(defmethod
  ^{:doc "Updates the lighting option used for the current scene."}
  event-tx-fn :scene/change-lighting
  [_ _ value]
  [[:db.fn/call assoc-scene :scene/lighting value]])

;; --- Objects ---
(defmethod event-tx-fn :objects/translate
  ^{:doc "Translates the object given by id by the given delta."}
  [_ _ id delta]
  [[:db.fn/call event-tx-fn :objects/translate-many #{id} delta]])

(def ^:private translate-many-select
  [:db/id
   :object/type
   :object/point
   [:object/locked :default false]
   :minigame/cards
   :memory/difficulty
   [:object/scale :default 1]
   [:object/rotation :default 0]
   :shape/points
   :token/size
   {:prop/image [:image/width :image/height :image/anchor]}
   {:board/image [:image/width :image/height :image/anchor]}])

(def ^:private snap-to-cell-types
  "Object types whose true center (not their :object/point, which for
   props/board pieces is a corner) snaps to the nearest grid-cell center --
   see geom/snap-to-cell. Tokens are included too: their :object/point
   already is their center, so snap-to-cell degenerates to exactly the
   hex/square math they've always used."
  #{:token/token :prop/prop :board/piece})

(defmethod event-tx-fn :objects/translate-many
  ^{:doc "Translates the objects given by idxs by the given delta,
          possibly aligning them to the grid if the appropriate scene
          option is enabled."}
  [data _ idxs delta]
  (let [result (ds/entity data [:db/ident :user])
        scene (-> result :user/camera :camera/scene)
        {align? :scene/grid-align
         grid-type :scene/grid-type} scene
        align? (and align? (pos? (game-type/grid-count (:game-type/enabled-elements (:scene/game-type scene) #{}))))
        base-type (geom/base-grid-type grid-type)]
    (into [[:db/retract [:db/ident :user] :user/dragging]]
          (for [entity (ds/pull-many data translate-many-select idxs)
                :let [{id :db/id point :object/point type :object/type} entity]
                ;; Skip locked objects, and anything already gone. The lock
                ;; was only ever enforced in the drag handler, and only for
                ;; a SINGLE-object selection -- rubber-band a locked object
                ;; together with any other and the group drag moved it
                ;; anyway. Skipping here rather than rejecting the whole
                ;; transaction gives the behaviour you'd want from a lock:
                ;; the rest of the selection moves, the locked member stays
                ;; put. The `point` check covers an id whose entity was
                ;; retracted mid-drag (ds/pull-many yields nil for it, and
                ;; vec/add on nil would throw and lose the whole move).
                :when (and (some? point)
                           (not (:object/locked entity))
                           ;; A mini-game table freezes while a game is on
                           ;; it -- dragging the board mid-game would move
                           ;; every card under the players at once.
                           (not (memory/table-in-play? entity)))]
            (cond
              (and align? (contains? snap-to-cell-types type))
              {:db/id id :object/point (geom/snap-to-cell entity delta base-type)}
              (and align? (not= type :note/note))
              (let [round (geom/object-alignment entity)]
                {:db/id id :object/point (vec/rnd (vec/add point delta) round)})
              :else
              {:db/id id :object/point (vec/add point delta)})))))

(defmethod event-tx-fn :objects/translate-selected
  ^{:doc "Translate the currently selected objects by the given delta."}
  [data _ delta]
  (let [select [{:user/camera [{:camera/selected [:db/id :object/point]}]}]
        result (ds/pull data select [:db/ident :user])
        {{selected :camera/selected} :user/camera} result]
    [[:db.fn/call event-tx-fn :objects/translate-many (into #{} (map :db/id) selected) delta]]))

(defmethod event-tx-fn :objects/select
  ^{:doc "Joins or removes the object given by id to the current selection,
          alternating behavior based on the boolean modify."}
  [data _ id modify]
  (let [object (ds/entity data id)
        entity (ds/entity data [:db/ident :user])
        {{camera :db/id selected :camera/selected} :user/camera} entity]
    [[:db/retract [:db/ident :user] :user/dragging]
     (if (not modify)
       [:db/retract camera :camera/selected])
     (if (and modify (contains? selected object))
       [:db/retract camera :camera/selected id]
       {:db/id camera :camera/selected {:db/id id}})]))

(defn ^:private authorized-to-own?
  "The ownership half of authorized-to-hide? below, on its own -- the
   host (unless the scene is :scene/neutral-authority?), or the
   *connected* controller assigned via :object/owner ->
   :player/controller. Factored out so the destructive actions can reuse
   the ownership rule without inheriting the :object/shared? escape
   hatch that only makes sense for a reversible toggle."
  [data entity]
  (let [user (ds/entity data [:db/ident :user])
        scene (:camera/scene (:user/camera user))
        default (and (:user/host user) (not (:scene/neutral-authority? scene)))
        connected (into #{} (map :user/uuid) (:session/conns (ds/entity data [:db/ident :session])))
        controller-uuid (get-in entity [:object/owner :player/controller :user/uuid])]
    (player/authority? (:user/uuid user) default connected controller-uuid)))

(defn ^:private authorized-to-remove?
  "True if the local viewer may DELETE `entity`. Deliberately
   authorized-to-own? WITHOUT authorized-to-hide?'s :object/shared?
   escape hatch: 'any connected participant may flip this shared card
   face-up' must not also mean 'any connected participant may destroy
   it'. Retraction is irreversible -- this app has no undo -- so it
   stays with the owner or the host."
  [data entity]
  (authorized-to-own? data entity))

(defn ^:private authorized-to-hide?
  "True if the local viewer may toggle :object/hidden for `entity` --
   the host, unless a *connected* controller is assigned to it via
   :object/owner -> :player/controller, in which case only that
   controller may (see ogres.app.player/authority?, the same primitive
   the render-time visibility filters use, so who may flip the switch
   and who's exempted from the hidden-filter always agree). An entity
   flagged :object/shared? true is an explicit opt-in escape hatch on
   top of that -- ANY connected participant may flip it (a 'public
   toggle' shared table object, e.g. a physical playing card any player
   may flip on their turn), regardless of owner/controller. Absent or
   false, behavior is exactly as before this flag existed.

   The host's own default authority is itself conditional on the
   current scene's :scene/neutral-authority? -- when true (an
   'impartial dealer' scene, e.g. a Memory game in progress, see
   :memory/start), the host gets NO automatic fallback either, so an
   unowned/unassigned object is authorized for NO ONE until explicitly
   owned/controlled or flagged :object/shared?. This is what keeps a
   host who's also playing from having an automatic, unfair advantage
   over hidden game state -- 'the host retains control of the room,
   not X-ray vision over gameplay.'"
  [data entity]
  (or (:object/shared? entity)
      (authorized-to-own? data entity)))

(defmethod
  ^{:doc "Hide or reveal the given object. The host may always do this
          unless a connected controller is assigned to it (see
          authorized-to-hide?), in which case only that controller may
          -- silently no-ops for anyone else, the same advisory
          defense-in-depth spirit as :player/change-color."}
  event-tx-fn :objects/toggle-hidden
  [data _ id]
  (let [entity (ds/entity data id)]
    (if (authorized-to-hide? data entity)
      [[:db/add id :object/hidden (not (:object/hidden entity))]]
      [])))

(defmethod
  ^{:doc "Hides or reveals the currently selected objects. Same
          per-object authorization as :objects/toggle-hidden -- objects
          the caller isn't authorized for are silently left untouched
          rather than blocking the rest of the selection."}
  event-tx-fn :objects/toggle-hidden-selected
  [data _]
  (let [user (ds/entity data [:db/ident :user])
        selected (:camera/selected (:user/camera user))
        target (not (every? :object/hidden selected))]
    (for [entity selected
          :when (authorized-to-hide? data entity)]
      [:db/add (:db/id entity) :object/hidden target])))

(defmethod
  ^{:doc "Locks or unlocks the currently selected objects."}
  event-tx-fn :objects/toggle-locked-selected
  [data _]
  (let [user (ds/entity data [:db/ident :user])
        selected (:camera/selected (:user/camera user))]
    (for [{id :db/id} selected]
      [:db/add id :object/locked (not (every? :object/locked selected))])))

(defmethod
  ^{:doc "Assigns (or, when player-id is nil, unassigns) the given
          objects' (tokens or props, by id) owning roster player -- the
          seam :objects/toggle-hidden(-selected) authorizes against via
          :player/controller (see authorized-to-hide?)."}
  event-tx-fn :objects/assign-owner
  [_ _ idxs player-id]
  (for [id idxs]
    (if player-id
      {:db/id id :object/owner player-id}
      [:db/retract id :object/owner])))

(defmethod
  ^{:doc "Sets (or, when hash is nil, clears) the given objects' (tokens
          or props, by id) placeholder image -- the image rendered in
          place of the real one for a viewer who lacks authority to see
          it while it's :object/hidden (see resolve-hidden,
          scene_objects.cljs). image-key is :token/image-alt or
          :prop/image-alt, matching the object's own type."}
  event-tx-fn :objects/assign-alt-image
  [_ _ idxs image-key hash]
  (for [id idxs]
    (if hash
      {:db/id id image-key [:image/hash hash]}
      [:db/retract id image-key])))

(defmethod
  ^{:doc "Sets (or clears) the given objects' (tokens or props, by id)
          :object/shared? flag -- an explicit per-object opt-in that lets
          ANY connected participant toggle its :object/hidden state, not
          just the host or its assigned owner's controller (see
          authorized-to-hide?). Defaults to unset/false, so this only
          ever loosens permission for objects that explicitly opt in;
          every other hidden object (e.g. a host's secret note) is
          completely unaffected."}
  event-tx-fn :objects/assign-shared
  [_ _ idxs shared?]
  (for [id idxs]
    (if shared?
      {:db/id id :object/shared? true}
      [:db/retract id :object/shared?])))

(defn ^:private merge-variables
  "The next :object/variables map for `entity` after merging `kvs` into
   it (creating the map if absent) -- a nil value in `kvs` removes that
   key rather than storing nil, the same 'nil clears' idiom
   :objects/assign-owner already uses for a whole attribute. Returns nil
   (not an empty map) if the result has no keys left, so callers can
   retract the attribute entirely instead of leaving an empty map
   behind."
  [entity kvs]
  (not-empty
   (reduce-kv (fn [m k v] (if (nil? v) (dissoc m k) (assoc m k v)))
              (or (:object/variables entity) {})
              kvs)))

(defmethod
  ^{:doc "Merges the given key-value pairs into each object's (tokens or
          props, by id) generic :object/variables map -- a nil value in
          kvs removes that key. Retracts :object/variables entirely if
          the merge leaves it empty. This is the one generic, arbitrary
          per-object data attachment point every other :props/* method
          in this file (piles, copies) builds on top of, rather than
          each inventing its own bespoke attribute."}
  event-tx-fn :objects/merge-variables
  [data _ idxs kvs]
  (for [id idxs
        :let [next (merge-variables (ds/entity data id) kvs)]]
    (if next
      {:db/id id :object/variables next}
      [:db/retract id :object/variables])))

(defmethod
  ^{:doc "Resets all transformations for the currently selected objects."}
  event-tx-fn :objects/reset-transform-selected
  [data _]
  (let [user (ds/entity data [:db/ident :user])]
    (into
     [] cat
     (for [{id :db/id} (:camera/selected (:user/camera user))]
       [[:db/retract id :object/scale]
        [:db/retract id :object/rotation]]))))

(defmethod
  ^{:doc "Change the scaling of the given object."}
  event-tx-fn :object/change-scale
  [_ _ id scale]
  [[:db/add id :object/scale scale]])

(defmethod
  ^{:doc "Change the rotation of the given object."}
  event-tx-fn :object/change-rotation
  [_ _ id rotation]
  [[:db/add id :object/rotation rotation]])

(defmethod
  ^{:doc "Change the rotation-snap mode of the given object -- :free, or a
          degree number (15/30/45/60/90) to hard-snap rotation drags to
          that increment. Currently used by board pieces; the available
          choices are filtered by grid family in the UI (see
          panel_scene.cljs)."}
  event-tx-fn :object/change-rotation-mode
  [_ _ id mode]
  [[:db/add id :object/rotation-mode mode]])

(defmethod
  ^{:doc "Toggles a layer-shift override on the given object -- :forward
          moves a prop to render above the token layer, :back moves a
          token to render below the prop layer (both still always render
          above the board layer). Toggling the same value again clears
          the override back to the default band for that type. Toggling
          it on a type that doesn't consult it at render time (see
          scene_objects.cljs's `objects` component) is a harmless no-op."}
  event-tx-fn :object/toggle-layer-shift
  [data _ id value]
  (let [entity (ds/entity data id)]
    [[:db/add id :object/layer-shift (if (= (:object/layer-shift entity) value) nil value)]]))

(defmethod
  ^{:doc "Toggles a layer-shift override (:forward or :back) on all
          currently selected objects at once -- if every selected object
          already has the given value, clears it back to nil (the default
          band for that type) on all of them, otherwise sets it on all of
          them. Mirrors the every?/not toggling convention already used by
          :objects/toggle-hidden-selected and :objects/toggle-locked-selected."}
  event-tx-fn :objects/toggle-layer-shift-selected
  [data _ value]
  (let [user (ds/entity data [:db/ident :user])
        selected (:camera/selected (:user/camera user))
        clear? (every? (comp #{value} :object/layer-shift) selected)]
    (for [{id :db/id} selected]
      [:db/add id :object/layer-shift (if clear? nil value)])))

(defmethod
  ^{:doc "Saves the given object's current effective scale as its source
          image's default cell-scale calibration (grid-size / scale =
          native-pixels-per-grid-cell), so future drops of that same image
          -- as either a prop or a board piece -- automatically scale to
          match the grid instead of dropping at native size. Retroactively
          re-scales every other already-placed instance of the same image
          too, across every scene, the same way saving a hex-scale in the
          predecessor project (worldhaven-asset-browser's builder tool)
          fixed up existing copies on its canvas."}
  event-tx-fn :image/set-cell-scale
  [data _ id]
  (let [entity (ds/entity data id)
        scale (:object/scale entity 1)
        hash (:image/hash (or (:prop/image entity) (:board/image entity)))
        cell-px (/ grid-size scale)
        root (ds/entity data [:db/ident :root])
        image-hash (fn [e] (:image/hash (or (:prop/image e) (:board/image e))))
        others (mapcat (fn [scene] (concat (:scene/props scene) (:scene/board scene)))
                       (:root/scenes root))]
    (into [[:db/add [:image/hash hash] :image/cell-px cell-px]]
          (for [other others :when (and (= hash (image-hash other)) (not= (:db/id other) id))]
            [:db/add (:db/id other) :object/scale scale]))))

(defmethod
  ^{:doc "Saves the given object's current rotation as its source image's
          default rotation, so future drops of that same image -- as
          either a prop or a board piece -- start out pre-rotated to
          match instead of always dropping at 0 degrees. Retroactively
          re-rotates every other already-placed instance of the same
          image too, across every scene, mirroring
          :image/set-cell-scale's identical retroactive-update
          behavior for scale."}
  event-tx-fn :image/set-rotation
  [data _ id]
  (let [entity (ds/entity data id)
        rotation (:object/rotation entity 0)
        hash (:image/hash (or (:prop/image entity) (:board/image entity)))
        root (ds/entity data [:db/ident :root])
        image-hash (fn [e] (:image/hash (or (:prop/image e) (:board/image e))))
        others (mapcat (fn [scene] (concat (:scene/props scene) (:scene/board scene)))
                       (:root/scenes root))]
    (into [[:db/add [:image/hash hash] :image/rotation rotation]]
          (for [other others :when (and (= hash (image-hash other)) (not= (:db/id other) id))]
            [:db/add (:db/id other) :object/rotation rotation]))))

(defmethod
  ^{:doc "Saves the given point (in the object's own local, unrotated,
          unscaled image-pixel space -- the same space as its width/height)
          as its source image's grid anchor -- the point within the
          artwork that should snap to a grid cell center, for images (like
          irregular jigsaw-shaped map tiles) whose true hex/square grid
          isn't centered on the plain bounding box. See geom/snap-to-cell
          and geom/object-anchor-point. Unlike :image/set-cell-scale, this
          intentionally does NOT retroactively move any other already-
          placed instance of the same image -- only future placements and
          re-alignments are affected."}
  event-tx-fn :image/set-anchor
  [data _ id local-point]
  (let [entity (ds/entity data id)
        hash (:image/hash (or (:prop/image entity) (:board/image entity)))]
    [[:db/add [:image/hash hash] :image/anchor local-point]]))

(defmethod
  ^{:doc "Removes the objects given by idxs."}
  event-tx-fn :objects/remove
  [data _ idxs]
  (for [id idxs
        :when (authorized-to-remove? data (ds/entity data id))]
    [:db/retractEntity id]))

(defmethod
  ^{:doc "Removes all currently selected objects the viewer is actually
          allowed to remove (see authorized-to-remove?) -- checked here
          rather than only disabled in the context menu, since any
          connected participant can dispatch this directly and every
          peer applies the broadcast datoms verbatim. Anything they may
          not remove is silently left alone, the same shape every other
          partially-authorized bulk action here takes (compare
          :objects/toggle-hidden-selected)."}
  event-tx-fn :objects/remove-selected
  [data _]
  (let [user (ds/entity data [:db/ident :user])]
    (for [entity (:camera/selected (:user/camera user))
          :when (authorized-to-remove? data entity)]
      [:db/retractEntity (:db/id entity)])))

(defmethod
  ^{:doc "Updates the attribute for the objects given by idxs to the
          given value."}
  event-tx-fn :objects/update
  [_ _ idxs attr value]
  (for [id idxs]
    (assoc {:db/id id} attr value)))

;; --- Tokens ---
(defmethod
  ^{:doc "Creates a new token on the current scene at the screen coordinates
          at the given point. These coordinates are converted to the
          scene coordinate space."}
  event-tx-fn :token/create
  [data _ point hash]
  (let [user  (ds/entity data [:db/ident :user])
        image (ds/entity data [:image/hash hash])
        {{camera :db/id
          shift :camera/point
          scale :camera/scale
          {scene :db/id
           align? :scene/grid-align
           grid-type :scene/grid-type
           game-type-entity :scene/game-type} :camera/scene} :user/camera} user
        align? (and align? (pos? (game-type/grid-count (:game-type/enabled-elements game-type-entity #{}))))
        base-type (geom/base-grid-type grid-type)
        point (vec/add (geom/screen->scene-vec point scale grid-type) shift)]
    (cond->
     [[:db/add scene :scene/tokens -1]
      [:db/add camera :camera/selected -1]
      [:db/add camera :camera/draw-mode :select]
      [:db/add -1 :object/type :token/token]]
      (some? image)
      (conj [:db/add -1 :token/image (:db/id image)])
      (and (some? image) (some? (:token-image/default-label image)))
      (conj [:db/add -1 :token/label (:token-image/default-label image)])
      (not align?)
      (conj [:db/add -1 :object/point point])
      (and align? (= base-type :hex-pointy))
      (conj [:db/add -1 :object/point (vec/nearest-hex point hex-radius)])
      (and align? (= base-type :hex-flat))
      (conj [:db/add -1 :object/point (vec/nearest-hex-flat point hex-radius)])
      (and align? (not (#{:hex-pointy :hex-flat} base-type)))
      (conj [:db/add -1 :object/point (vec/nearest-square point grid-size)]))))

(defmethod event-tx-fn :token/change-flag
  [data _ idxs flag add?]
  (let [tokens (ds/pull-many data [:db/id :token/flags] idxs)]
    (for [{:keys [db/id token/flags] :or {flags #{}}} tokens]
      {:db/id id :token/flags ((if add? conj disj) flags flag)})))

(defmethod event-tx-fn :token/clear-flags
  ;; Clears exactly the flags named in `values` -- the caller's whole
  ;; vocabulary, see game-type.widgets/status-checklist's "clear all".
  ;; Takes them explicitly rather than emptying :token/flags, because
  ;; that one set also carries :player and :dead, which are toggled from
  ;; the context menu's own toolbar and have nothing to do with a
  ;; game-type's conditions -- wiping the set would silently strip a
  ;; player token of its player-ness. Doing it in one transaction rather
  ;; than a :token/change-flag per condition means one broadcast to
  ;; connected peers instead of N.
  [data _ idxs values]
  (let [tokens   (ds/pull-many data [:db/id :token/flags] idxs)
        clearing (set values)]
    (for [{:keys [db/id token/flags] :or {flags #{}}} tokens]
      {:db/id id :token/flags (into #{} (remove clearing) flags)})))

(defmethod event-tx-fn :token/change-label
  [_ _ idxs value]
  (for [id idxs]
    {:db/id id :token/label (trim value)}))

(defmethod event-tx-fn :token/change-size
  [_ _ idxs radius]
  (for [id idxs]
    {:db/id id :token/size radius}))

(defmethod event-tx-fn :token/change-light
  [_ _ idxs radius]
  (for [id idxs]
    {:db/id id :token/light radius}))

(defmethod event-tx-fn :token/change-aura
  [_ _ idxs radius]
  (for [id idxs]
    {:db/id id :token/aura-radius radius}))

(defmethod event-tx-fn :token/change-dead
  [_ _ idxs add?]
  [[:db.fn/call event-tx-fn :token/change-flag idxs :dead add?]
   (if add?
     [:db.fn/call event-tx-fn :initiative/toggle idxs false])])

(defmethod event-tx-fn :shape/create
  [_ _ type [src & points]]
  [{:db/id -1
    :object/type (keyword :shape type)
    :object/point src
    :shape/points (into [] (map (fn [vrt] (vec/sub vrt src))) points)}
   [:db.fn/call assoc-camera :camera/draw-mode :select :camera/selected -1]
   [:db.fn/call assoc-scene :scene/shapes -1]])

(defmethod event-tx-fn :user/change-bounds
  [_ _ bounds]
  [[:db/add -1 :db/ident :user]
   [:db/add -1 :user/bounds bounds]])

(defmethod event-tx-fn :selection/from-rect
  [data _ rect]
  (let [result (ds/entity data [:db/ident :root])
        {{{{tokens :scene/tokens
            shapes :scene/shapes
            notes  :scene/notes
            props  :scene/props} :camera/scene
           camera :db/id} :user/camera
          host :user/host} :root/user
         {conns :session/conns} :root/session} result
        bounds (geom/bounding-rect (seq rect))
        occupied (into #{} (comp (mapcat :user/dragging) (map :db/id)) conns)]
    [{:db/id camera
      :camera/draw-mode :select
      :camera/selected
      (for [entity (concat shapes tokens notes props)
            :let   [{id :db/id} entity]
            :let   [object (geom/object-bounding-rect entity)]
            :when  (and (not (occupied id))
                        (not (and (not host) (:object/hidden entity)))
                        (not (and (not host) (= (:object/type entity) :note/note)))
                        (not (and (not host) (= (:object/type entity) :prop/prop)))
                        (geom/rect-intersects-rect object bounds))]
        {:db/id id})}]))

(defmethod event-tx-fn :selection/clear
  [data]
  (let [user (ds/entity data [:db/ident :user])]
    [[:db/retract (:db/id (:user/camera user)) :camera/selected]]))

(defmethod event-tx-fn :selection/remove
  [data]
  (let [user (ds/entity data [:db/ident :user])]
    (for [entity (:camera/selected (:user/camera user))]
      [:db/retractEntity (:db/id entity)])))

(defmethod event-tx-fn :initiative/toggle
  [data _ idxs adding?]
  (let [user   (ds/entity data [:db/ident :user])
        scene  (:db/id (:camera/scene (:user/camera user)))
        select [:db/id {:token/image [:image/hash]} [:token/flags :default #{}] :initiative/suffix :token/label]
        result (ds/pull data [{:scene/initiative select}] scene)
        change (into #{} (ds/pull-many data select idxs))
        exists (into #{} (:scene/initiative result))]
    (if adding?
      [{:db/id scene
        :scene/initiative
        (let [merge (union exists change)
              sffxs (suffixes merge)]
          (for [token merge :let [id (:db/id token)]]
            (if-let [suffix (sffxs id)]
              {:db/id id :initiative/suffix suffix}
              {:db/id id})))}]
      (into [] cat
            (for [{id :db/id} change]
              [[:db/retract id :initiative/suffix]
               [:db/retract id :initiative/rank]
               [:db/retract id :initiative/health]
               [:db/retract scene :scene/initiative id]
               [:db/retract scene :initiative/played id]])))))

(defmethod event-tx-fn :initiative/next
  [data]
  (let [user (ds/entity data [:db/ident :user])
        scene (:camera/scene (:user/camera user))
        {rounds :initiative/rounds
         tokens :scene/initiative
         played :initiative/played} scene]
    (if (some? rounds)
      (let [[next] (sort initiative-order (difference tokens played))]
        (if (some? next)
          [{:db/id (:db/id scene)
            :initiative/turn (:db/id next)
            :initiative/played (:db/id next)}]
          [[:db/retract (:db/id scene) :initiative/played]
           [:db/retract (:db/id scene) :initiative/turn]
           {:db/id (:db/id scene) :initiative/rounds (inc rounds)}]))
      [{:db/id (:db/id scene) :initiative/rounds 1}])))

(defmethod event-tx-fn :initiative/mark
  [data _ id]
  (let [user (ds/entity data [:db/ident :user])
        scene (:camera/scene (:user/camera user))]
    [{:db/id (:db/id scene)
      :initiative/turn {:db/id id}
      :initiative/played {:db/id id}
      :initiative/rounds (max (:initiative/rounds scene) 1)}]))

(defmethod event-tx-fn :initiative/unmark
  [data _ id]
  (let [user (ds/entity data [:db/ident :user])
        scene (:camera/scene (:user/camera user))]
    [[:db/retract (:db/id scene) :initiative/played id]
     (if (= (:db/id (:initiative/turn scene)) id)
       [:db/retract (:db/id scene) :initiative/turn])]))

(defmethod
  ^{:doc "Sets or clears one token's :initiative/rank -- the base,
          game-agnostic turn-order value. Blank/nil clears it back to
          unranked; anything else is parsed as a number. Doesn't care
          whether the caller is a manual number entry, the
          :initiative/move nudge below, or a game module's own mechanism
          (e.g. D&D's d20 roll writes here too, see
          ogres.app.game-type.games.dnd5e) -- this event has no opinion
          about how the value was chosen."}
  event-tx-fn :initiative/change-rank
  [_ _ id rank]
  (let [parsed (js/parseFloat rank)]
    (cond
      (or (nil? rank) (= rank ""))
      [[:db/retract id :initiative/rank]]

      (.isNaN js/Number parsed)
      []

      :else
      [{:db/id id :initiative/rank parsed}])))

(defmethod
  ^{:doc "Batch-writes an explicit :initiative/rank for each entry in the
          given `id->rank` map, in a single transaction -- one re-render,
          one transaction (and so one broadcast), instead of N separate :initiative/change-rank
          dispatches. Contains no opinion about how those values were
          chosen; a game module decides the values (e.g. D&D's d20 roll
          for every un-ranked NPC at once, see
          ogres.app.game-type.games.dnd5e's :initiative-actions
          contribution) and this just writes them."}
  event-tx-fn :initiative/assign-ranks
  [_ _ id->rank]
  (for [[id rank] id->rank]
    {:db/id id :initiative/rank rank}))

(defmethod
  ^{:doc "Shifts the given token one position :earlier or :later in the
          current turn order (a no-op if it's already at that end), then
          renumbers *every* participant's :initiative/rank by their final
          list position -- always producing a clean contiguous descending
          block, regardless of whether ranks were previously nil,
          manually set, or set by a game module's own mechanism. This is
          the base, manual way to assign turn order: available for every
          game-type, not gated behind any element. Note: on a scene where
          nobody has a rank yet, the first move assigns one to *every*
          participant at once (since the whole list gets renumbered) --
          expected, not a bug, but it means a token nudged into place
          this way will read as already-ranked to any module's bulk-
          assignment eligibility check (e.g. widgets/unranked-npc?)."}
  event-tx-fn :initiative/move
  [data _ id direction]
  (let [user (ds/entity data [:db/ident :user])
        scene (:camera/scene (:user/camera user))
        ordered (vec (sort initiative-order (:scene/initiative scene)))
        index (first (keep-indexed (fn [i token] (if (= (:db/id token) id) i)) ordered))]
    (if (nil? index)
      []
      (let [target (case direction :earlier (dec index) :later (inc index))]
        (if (or (neg? target) (>= target (count ordered)))
          []
          (let [reordered (assoc ordered index (ordered target) target (ordered index))
                n (count reordered)]
            (map-indexed (fn [i token] {:db/id (:db/id token) :initiative/rank (- n i)}) reordered)))))))

(defmethod event-tx-fn :initiative/change-health
  [data _ id f value]
  (let [parsed (.parseFloat js/window value)]
    (if (.isNaN js/Number parsed) []
        (let [{:keys [initiative/health]} (ds/entity data id)]
          [{:db/id id :initiative/health (f health parsed)}]))))

(defmethod event-tx-fn :initiative/leave
  [data]
  (let [user (ds/entity data [:db/ident :user])
        scene (:camera/scene (:user/camera user))]
    (apply concat
           [[:db/retract (:db/id scene) :scene/initiative]
            [:db/retract (:db/id scene) :initiative/turn]
            [:db/retract (:db/id scene) :initiative/played]
            [:db/retract (:db/id scene) :initiative/rounds]]
           (for [{id :db/id} (:scene/initiative scene)]
             [[:db/retract id :initiative/rank]
              [:db/retract id :initiative/health]
              [:db/retract id :initiative/suffix]]))))

;; --- Cards / Decks ---
(defn ^:private pile
  "The subset of a (pulled) deck's :deck/cards currently in `location`
   (:draw, :discard, or :hand)."
  [deck location]
  (filter (comp #{location} :card/location) (:deck/cards deck)))

(defn ^:private top-card
  "The card with the highest :card/position among `cards` -- 'top of the
   pile' by the same position-as-order-key idiom :initiative/rank uses."
  [cards]
  (apply max-key :card/position cards))

(defn ^:private next-position
  "The position a newly-arriving card should take to land on top of
   `cards` -- one past the current max, or 0 for an empty pile."
  [cards]
  (if (seq cards) (inc (apply max (map :card/position cards))) 0))

(defn ^:private move-card-tx
  "Tx-data moving one card to `location` at `position`, setting
   :card/holder when `holder` is given (moving into a hand) or explicitly
   retracting it otherwise (moving out of one, or it was never set)."
  [card-id location position holder]
  (if (some? holder)
    [{:db/id card-id :card/location location :card/position position :card/holder holder}]
    [{:db/id card-id :card/location location :card/position position}
     [:db/retract card-id :card/holder]]))

(defn ^:private deck-create-tx
  "Tx-data (paired with the deck's own :db/id, as [deck-id tx-data])
   creating a new deck instance from a registered definition (see
   game-type/deck-definitions), shuffled into its draw pile immediately
   -- the core of :deck/create, factored out as a plain function (not
   an event-tx-fn) so another event that needs to create a deck AND
   immediately reference its :db/id within its OWN single transaction
   (e.g. :go-fish/start dealing hands from the deck it just created)
   can call it directly and get the id back synchronously, rather than
   going through :deck/create's own opaque :db.fn/call indirection.
   Extras (e.g. jokers) are included only when include-extras? is true
   -- excluded by default, since they're not part of a deck's 'real'
   count. The optional label overrides the definition's own display
   name for this particular instance (e.g. distinguishing two decks
   built from the same template)."
  [deck-key {:keys [include-extras? label]}]
  (let [definition (get game-type/deck-definitions deck-key)
        cards-data (cond-> (vec (:deck/cards definition))
                     include-extras? (into (:deck/extras definition)))
        n (count cards-data)
        ids (mapv - (range 1 (inc n)))
        positions (cards/shuffle-positions ids)
        deck-id (dec (apply min ids))
        card-tx (map (fn [card id]
                       (assoc card :db/id id :card/location :draw :card/position (get positions id)))
                     cards-data ids)]
    [deck-id
     (concat [{:db/id deck-id
               :deck/name (or label (:deck/name definition))
               :deck/cards ids}]
             card-tx)]))

(defn ^:private deal-assignment
  "Round-robin assigns cards from the top (highest :card/position) of
   `cards` to each of `holder-ids` -- exactly `n` cards each if given,
   or every remaining card in `cards` if nil, some holders getting one
   more than others when it doesn't divide evenly (the same way
   dealing a physical deck around a table works). Returns
   {holder-id [card...]}, the cards themselves unchanged -- a plain
   in-memory result, NOT tx-data, so a caller needing to reason about
   the resulting hands before finalizing tx-data (e.g. Old Maid's
   auto-discard-of-pairs-at-deal-time) can. `cards` is typically a
   deck's own just-generated, not-yet-committed card tx-data (see
   deck-create-tx), not a deck already resolved in `data` -- a freshly
   created deck's :db/id is still a temp id at this point in the same
   transaction, which ds/entity (what :deck/draw/:deck/deal both need
   to re-read the CURRENT draw pile) can't resolve, only a real id
   can; building the deal directly from `cards` (plain Clojure data
   already in hand) sidesteps that entirely."
  [cards holder-ids n]
  (let [ordered (sort-by :card/position > cards)
        ordered (cond->> ordered n (take (* n (count holder-ids))))]
    (->> (map vector ordered (cycle holder-ids))
         (group-by second)
         (into {} (map (fn [[holder-id pairs]] [holder-id (mapv first pairs)]))))))

(defn ^:private deal-tx
  "Tx-data dealing every card `deal-assignment` assigns straight into
   its holder's hand -- used by :go-fish/start, where everyone simply
   keeps whatever they're dealt (contrast Old Maid's :old-maid/start,
   which post-processes a deal-assignment result to also auto-discard
   pairs before anything is asserted into a hand at all)."
  [cards holder-ids n]
  (mapcat (fn [[holder-id cards]]
            (map-indexed
             (fn [i card]
               {:db/id (:db/id card) :card/location :hand
                :card/holder holder-id :card/position i})
             cards))
          (deal-assignment cards holder-ids n)))

(defn ^:private effective-draw-pile
  "`deck`'s current :draw pile, reshuffled from :discard (minus its
   own live top card) first if :draw is empty -- {:cards [...]
   :reshuffle-tx [...]}, :cards reflecting whichever is actually live
   right now (plain data, :card/position already updated if a
   reshuffle just happened, so a caller can pick its own top card
   straight off it without re-querying `data` -- the reshuffle tx-data
   hasn't committed yet, the same 'return plain data a caller reasons
   about before finalizing tx' idiom deal-assignment already uses).
   The shared 'protect the live discard top from the reshuffle' rule
   Crazy 8s and Rummy both need (a card matching/scoring is checked
   against still has to exist somewhere) -- unlike generic :deck/
   draw's own reshuffle, which reclaims the WHOLE discard pile, since
   a plain deck browser has no 'still in play' top card to protect."
  [deck]
  (let [draw (pile deck :draw)]
    (if (seq draw)
      {:cards draw :reshuffle-tx []}
      (let [discard (pile deck :discard)
            top (top-card discard)
            pool (remove (comp #{(:db/id top)} :db/id) discard)]
        (if (seq pool)
          (let [positions (cards/shuffle-positions (map :db/id pool))
                ;; `pool` entries are DataScript entities (deck is
                ;; already-committed, unlike deal-assignment's usual
                ;; not-yet-committed plain-map input) -- assoc doesn't
                ;; work on those directly, so build fresh plain maps.
                shuffled (map (fn [c] {:db/id (:db/id c) :card/location :draw
                                        :card/position (get positions (:db/id c))})
                               pool)]
            {:cards shuffled :reshuffle-tx shuffled})
          {:cards [] :reshuffle-tx []})))))

(defmethod
  ^{:doc "Creates a new deck instance on the current scene from a
          registered deck definition (see game-type/deck-definitions),
          shuffled into its draw pile immediately. Extras (e.g. jokers)
          are included only when include-extras? is true -- excluded by
          default, since they're not part of a deck's 'real' count. The
          optional label overrides the definition's own display name for
          this particular instance (e.g. distinguishing two decks built
          from the same template)."}
  event-tx-fn :deck/create
  ([data event deck-key]
   [[:db.fn/call event-tx-fn event deck-key {}]])
  ([_ _ deck-key opts]
   (let [[deck-id tx] (deck-create-tx deck-key opts)]
     (conj (vec tx) [:db.fn/call assoc-scene :scene/decks deck-id]))))

(defmethod
  ^{:doc "Reassigns fresh random positions to every card currently in the
          given deck's draw pile -- discard and any hands are untouched.
          :deck/create already shuffles a fresh deck, so this is for
          re-shuffling later (e.g. a manual 'reshuffle' action)."}
  event-tx-fn :deck/shuffle
  [data _ deck-id]
  (let [deck (ds/entity data deck-id)
        ids (map :db/id (pile deck :draw))
        positions (cards/shuffle-positions ids)]
    (for [id ids] {:db/id id :card/position (get positions id)})))

(defmethod
  ^{:doc "Draws the top card of the given deck's draw pile to `target` --
          :discard (default) or [:hand holder-id]. If the draw pile is
          empty and the discard pile has cards to reclaim (see
          ogres.app.cards/needs-reshuffle?), reshuffles the whole discard
          pile back into the draw pile first, then draws from that in the
          same transaction -- this default rule (reshuffle only once the
          draw pile is truly empty) is the seam a later Gloomhaven-
          specific rule (reshuffle triggered early by a flagged card)
          would replace. A no-op if there's truly nothing left to draw."}
  event-tx-fn :deck/draw
  ([data event deck-id]
   [[:db.fn/call event-tx-fn event deck-id :discard]])
  ([data _ deck-id target]
   (let [deck (ds/entity data deck-id)
         draw (pile deck :draw)
         discard (pile deck :discard)]
     (cond
       (cards/needs-reshuffle? draw discard)
       (let [ids (map :db/id discard)
             positions (cards/shuffle-positions ids)]
         (concat
          (for [id ids] {:db/id id :card/location :draw :card/position (get positions id)})
          [[:db.fn/call event-tx-fn :deck/draw deck-id target]]))

       (seq draw)
       (let [top (:db/id (top-card draw))
             [location holder] (if (vector? target) [:hand (second target)] [target nil])
             dest (pile deck location)
             dest (if holder (filter (comp #{holder} :db/id :card/holder) dest) dest)]
         (move-card-tx top location (next-position dest) holder))

       :else []))))

(defmethod
  ^{:doc "Moves one card (typically drawn into a hand earlier) to its own
          deck's discard pile, on top."}
  event-tx-fn :deck/discard
  [data _ card-id]
  (let [card (ds/entity data card-id)
        deck (:deck/_cards card)]
    (move-card-tx card-id :discard (next-position (pile deck :discard)) nil)))

(defmethod
  ^{:doc "Draws n cards into each of the given holders' hands, one
          transaction -- wraps the same draw logic :deck/draw uses,
          batched across every holder."}
  event-tx-fn :deck/deal
  [_ _ deck-id holder-ids n]
  (apply concat
         (for [holder-id holder-ids _ (range n)]
           [[:db.fn/call event-tx-fn :deck/draw deck-id [:hand holder-id]]])))

(defmethod
  ^{:doc "Moves every card (draw + discard + every hand) for the given
          deck back to a freshly-shuffled draw pile -- a 'start over'
          action."}
  event-tx-fn :deck/reset
  [data _ deck-id]
  (let [deck (ds/entity data deck-id)
        ids (map :db/id (:deck/cards deck))
        positions (cards/shuffle-positions ids)]
    (into [] (mapcat (fn [id] (move-card-tx id :draw (get positions id) nil))) ids)))

(defmethod
  ^{:doc "Removes the given deck instance and all its cards (isComponent
          cleanup, same as removing any other owned collection in this
          schema)."}
  event-tx-fn :deck/remove
  [_ _ deck-id]
  [[:db/retractEntity deck-id]])

;; --- Attack Modifier Decks ---
;; The Gloomhaven-family ("x-haven") attack modifier deck mechanic -- see
;; ogres.app.attack-deck for the pure kind-vocabulary/comparison logic.
;; Each player has their own personal 20-card deck, reached via a FORWARD
;; ref from the player (:player/attack-deck) -- root-scoped, not scene-
;; scoped, so it survives a scene change/new scenario exactly the way
;; :root/players itself already does (this is the fix for the deck
;; otherwise resetting to vanilla every time a new scene starts). Monsters
;; share exactly one deck per scene (:scene/monster-attack-deck) -- a
;; fresh scenario's monsters really are a new deck, so THAT stays scene-
;; scoped on purpose. A whole new event family, NOT built on the generic
;; :deck/* methods above -- the round-boundary flagged-card reshuffle
;; rule and the draw-2-keep-better/worse shape of Advantage/Disadvantage
;; are different enough control flow that forcing reuse would fight the
;; existing code rather than share it -- but it still reuses this
;; namespace's own `pile`/`top-card`/`next-position`/`move-card-tx`
;; helpers and ogres.app.cards/shuffle-positions directly, and reuses the
;; existing :card/rank/:card/location/:card/position schema wholesale: a
;; card's :card/rank simply holds one of attack-deck's 9 kind keywords
;; instead of a playing-card rank, the same re-skinning precedent Old
;; Maid's own queen card already established.

(defn ^:private attack-deck-owner-authorized?
  "Whether `user` may act on behalf of `owner-id` (a specific roster
   player) or, if nil, the shared monster deck (host-only) -- the
   authority check shared by :attack-deck/create (no deck entity exists
   yet to resolve an owner from) and attack-deck-authorized? below (which
   resolves an owner-id from an EXISTING deck via a reverse ref, since
   there's no other way to ask 'who owns this deck' once ownership is a
   forward ref FROM the player rather than a stored :deck/owner)."
  [data user owner-id]
  (if owner-id
    (let [connected (into #{} (map :user/uuid) (:session/conns (ds/entity data [:db/ident :session])))
          controller-uuid (get-in (ds/entity data owner-id) [:player/controller :user/uuid])]
      (player/authority? (:user/uuid user) (:user/host user) connected controller-uuid))
    (:user/host user)))

(defn ^:private attack-deck-authorized?
  "Whether `user` may draw from or edit `deck`. Resolves the owning
   roster player via the :player/_attack-deck reverse ref -- a personal
   deck (some owner found) requires player/authority? over that specific
   player, the same explicit-target-id-plus-authority-check shape :dice/
   roll's own owner check uses. The shared monster deck (no owner found
   -- nothing points to it via :player/_attack-deck) requires the host --
   unlike a neutral dice roll (which anyone may make), running the
   monsters' turn is a GM action."
  [data user deck]
  (attack-deck-owner-authorized? data user (:db/id (:player/_attack-deck deck))))

(defn ^:private attack-deck-composition-tx
  "Tx-data (paired with the fresh cards' own :db/ids) for a freshly-dealt
   standard-composition set of cards, shuffled -- the seed both
   :attack-deck/create and :attack-deck/reset use."
  []
  (let [kinds (mapcat (fn [[kind n]] (repeat n kind)) attack-deck/standard-composition)
        n (count kinds)
        ids (mapv - (range 1 (inc n)))
        positions (cards/shuffle-positions ids)]
    [ids
     (map (fn [kind id] {:db/id id :card/rank kind :card/location :draw :card/position (get positions id)})
          kinds ids)]))

(defn ^:private pop-cards
  "Pops up to `n` cards off the top of `deck`'s current draw pile,
   reshuffling the discard pile back in (following the same reactive
   'draw pile empty, discard has cards to reclaim' rule ogres.app.cards/
   needs-reshuffle? already captures) as many times as needed in between
   -- {:picked [...entities, draw order...] :reshuffle-tx [...]}. Fewer
   than `n` picked only means the deck is truly out of cards altogether
   (draw and discard both empty). `:reshuffle-tx` is the tx-data
   realizing whichever reshuffle(s) actually happened, meant to be
   included ahead of whatever tx-data moves/retracts the picked cards."
  [deck n]
  (loop [draw (sort-by :card/position > (pile deck :draw))
         discard (pile deck :discard)
         picked []
         reshuffle-tx []]
    (cond
      (= (count picked) n)
      {:picked picked :reshuffle-tx reshuffle-tx}

      (seq draw)
      (recur (rest draw) discard (conj picked (first draw)) reshuffle-tx)

      (seq discard)
      (let [ids (map :db/id discard)
            positions (cards/shuffle-positions ids)
            tx (for [id ids] {:db/id id :card/location :draw :card/position (get positions id)})
            ;; `discard` entries are DataScript entities, not plain maps --
            ;; assoc doesn't work on those directly (see effective-draw-
            ;; pile's own identical note above), so build fresh plain maps.
            ;; Carries :card/effect/:card/effect-amount through too, same
            ;; as :card/rank -- otherwise a card reshuffled and drawn back
            ;; out within this same operation would silently lose its
            ;; attached effect.
            reshuffled (map (fn [c] (cond-> {:db/id (:db/id c) :card/rank (:card/rank c)
                                              :card/position (get positions (:db/id c))}
                                       (:card/effect c) (assoc :card/effect (:card/effect c))
                                       (:card/effect-amount c) (assoc :card/effect-amount (:card/effect-amount c))))
                             discard)]
        (recur (sort-by :card/position > reshuffled) [] picked (into reshuffle-tx tx)))

      :else
      {:picked picked :reshuffle-tx reshuffle-tx})))

(defn ^:private attack-deck-pick
  "Which of two just-drawn cards `mode` keeps -- the numerically better
   one for :advantage, the worse for :disadvantage (see ogres.app.attack-
   deck/better and /worse), ties favoring `a` (whichever was drawn
   first)."
  [mode [a b]]
  (let [ka (:card/rank a) kb (:card/rank b)]
    (case mode
      :advantage (if (= (attack-deck/better ka kb) ka) a b)
      :disadvantage (if (= (attack-deck/worse ka kb) ka) a b))))

(defmethod
  ^{:doc "Creates a new attack modifier deck, seeded from ogres.app.
          attack-deck/standard-composition and shuffled. `owner-id` nil
          creates the CURRENT SCENE's shared 'Monsters' deck (host-only,
          see attack-deck-owner-authorized?), scene-scoped since a fresh
          scenario's monsters really are a new deck; otherwise a
          PERSONAL deck attached directly to that roster player
          (:player/attack-deck, root-scoped -- that player, or the host,
          may create it), surviving every future scene change. A no-op
          if that owner (nil included) already has a deck -- exactly one
          deck per player, exactly one monster deck per scene. Also a
          no-op if :gloomhaven/attack-deck isn't actually enabled on the
          scene's own game-type (checked here server-side, not just
          gated in the UI, since any connected participant may dispatch
          this) -- the same enabled-elements check every ported mini-
          game's own /start requires for its own :X/game element (see
          :old-maid/start)."}
  event-tx-fn :attack-deck/create
  [data _ owner-id]
  (let [user (ds/entity data [:db/ident :user])
        scene (:camera/scene (:user/camera user))
        enabled (:game-type/enabled-elements (:scene/game-type scene))
        already-exists? (if owner-id
                          (some? (:player/attack-deck (ds/entity data owner-id)))
                          (some? (:scene/monster-attack-deck scene)))]
    (if (or (not (contains? enabled :gloomhaven/attack-deck))
            (not (attack-deck-owner-authorized? data user owner-id))
            already-exists?)
      []
      (let [label (if owner-id (:player/name (ds/entity data owner-id)) "Monsters")
            [ids card-tx] (attack-deck-composition-tx)
            deck-id (dec (apply min ids))]
        (concat [{:db/id deck-id :deck/name label :deck/cards ids}]
                card-tx
                (if owner-id
                  [[:db/add owner-id :player/attack-deck deck-id]]
                  [[:db.fn/call assoc-scene :scene/monster-attack-deck deck-id]]))))))

(defn ^:private attack-deck-shuffle-icons-enabled?
  "Whether the standard Null/2x reshuffle-icon rule is active on the
   current scene (:scene/attack-deck-shuffle-icons?, absent/true = the
   standard rule) -- see :attack-deck/toggle-shuffle-icons."
  [data]
  (let [scene (:camera/scene (:user/camera (ds/entity data [:db/ident :user])))]
    (not (false? (:scene/attack-deck-shuffle-icons? scene)))))

(defmethod
  ^{:doc "Draws from `deck-id`'s attack modifier deck -- 1 card normally,
          2 under :advantage/:disadvantage (`mode`), keeping the better
          or worse per attack-deck-pick, ties favoring whichever was
          drawn first. Reshuffles reactively (see pop-cards) if the draw
          pile runs out partway through, even mid-Advantage/Disadvantage.
          A drawn BLESS/CURSE card is retracted outright rather than
          discarded (ogres.app.attack-deck/removed-on-draw?); every other
          kind -- including the 'other' card under Advantage/Disadvantage,
          which is drawn too, just not applied -- moves to discard. Sets
          :deck/needs-reshuffle? true if EITHER drawn card was :null/
          :times-2 (see attack-deck/shuffle-triggering?) AND the
          reshuffle-icon rule is currently enabled (attack-deck-shuffle-
          icons-enabled?) -- the flag :attack-deck/reshuffle-flagged later
          sweeps at round end. Every draw appends one immutable :scene/
          attack-draws entry, the same 'one action, one history entity'
          shape :dice/roll's own :scene/dice-rolls uses -- carrying
          through :card/effect/:card/effect-amount (see :attack-deck/
          add-effect-cards) as :draw/effect(-amount), and the same for
          the discarded alternative under Advantage/Disadvantage, as
          :draw/discarded-effect(-amount). A no-op if the deck is truly
          empty (draw and discard both exhausted) or the viewer lacks
          attack-deck-authorized? over it."}
  event-tx-fn :attack-deck/draw
  [data _ deck-id mode]
  (let [user (ds/entity data [:db/ident :user])
        deck (ds/entity data deck-id)]
    (if-not (attack-deck-authorized? data user deck)
      []
      (let [n (if mode 2 1)
            {:keys [picked reshuffle-tx]} (pop-cards deck n)]
        (if (empty? picked)
          []
          (let [paired? (= (count picked) 2)
                kept (if paired? (attack-deck-pick mode picked) (first picked))
                other (if paired? (first (remove #{kept} picked)))
                flagged? (and (attack-deck-shuffle-icons-enabled? data)
                              (some (comp attack-deck/shuffle-triggering? :card/rank) picked))
                reshuffled? (seq reshuffle-tx)
                base (if reshuffled? 0 (next-position (pile deck :discard)))
                discardable (remove (comp attack-deck/removed-on-draw? :card/rank) picked)
                positions (zipmap discardable (range base (+ base (count discardable))))
                move-tx
                (mapcat
                 (fn [c]
                   (if (attack-deck/removed-on-draw? (:card/rank c))
                     [[:db/retractEntity (:db/id c)]]
                     (move-card-tx (:db/id c) :discard (get positions c) nil)))
                 picked)
                draw-id -1000
                draw-tx (cond-> {:db/id draw-id
                                 :draw/deck deck-id
                                 :draw/kind (:card/rank kept)
                                 :draw/at (.now js/Date)}
                          mode (assoc :draw/mode mode)
                          (:card/effect kept) (assoc :draw/effect (:card/effect kept))
                          (:card/effect-amount kept) (assoc :draw/effect-amount (:card/effect-amount kept))
                          other (assoc :draw/discarded-kind (:card/rank other))
                          (:card/effect other) (assoc :draw/discarded-effect (:card/effect other))
                          (:card/effect-amount other) (assoc :draw/discarded-effect-amount (:card/effect-amount other)))]
            (concat reshuffle-tx move-tx
                    (if flagged? [{:db/id deck-id :deck/needs-reshuffle? true}])
                    [draw-tx]
                    [[:db.fn/call assoc-scene :scene/attack-draws draw-id]])))))))

(defmethod
  ^{:doc "Updates whether drawing a Null/2x card flags a deck for
          reshuffle at all (:scene/attack-deck-shuffle-icons?, absent/
          true = the standard rule is active) -- an optional-rule
          toggle, the same shape :scene/toggle-neutral-authority already
          uses for its own scene-wide rule flag. When turned off,
          :attack-deck/draw never sets :deck/needs-reshuffle?, so
          :attack-deck/reshuffle-flagged naturally has nothing left to
          sweep."}
  event-tx-fn :attack-deck/toggle-shuffle-icons
  [_ _ enabled]
  [[:db.fn/call assoc-scene :scene/attack-deck-shuffle-icons? enabled]])

(defmethod
  ^{:doc "Updates the 'Reduced Randomness' variant (p.49) --
          :scene/attack-deck-reduced-randomness?, absent/false = normal.
          Purely a display simplification: parfaits never automates
          damage math for any mechanic, so there's nothing to actually
          recompute -- component/panel_attack_deck.cljs just labels a
          drawn/last-drawn :times-2/:bless as '+2' and :null/:curse as
          '-2' instead of their normal presentation when this is on.
          Draw/reshuffle/Advantage-Disadvantage logic is completely
          unaffected -- the rulebook is explicit decks still reshuffle
          at end of round after one of these cards is drawn regardless
          of this toggle. Same trust-the-UI shape as :scene/toggle-
          neutral-authority, since this is cosmetic, not destructive."}
  event-tx-fn :attack-deck/toggle-reduced-randomness
  [_ _ enabled]
  [[:db.fn/call assoc-scene :scene/attack-deck-reduced-randomness? enabled]])

(defn ^:private attack-deck-new-cards-tx
  "Tx-data adding `n` fresh cards of `kind` (optionally carrying
   `effect`/`amount`, both nil for a plain card) to `deck`'s draw pile,
   reshuffling the whole draw pile's positions afterward so the new
   cards land somewhere random rather than always on top -- the shared
   card-construction helper both :attack-deck/add-cards and :attack-
   deck/add-effect-cards build on. `temporary?` marks the new cards
   :card/temporary? -- scenario-scoped (item/scenario-effect-added, or
   BLESS/CURSE -- see :attack-deck/end-scenario), swept at end of
   scenario regardless of whether they were ever drawn, unlike a
   permanent perk-added card."
  [deck kind n effect amount temporary?]
  (let [existing-ids (map :db/id (pile deck :draw))
        new-ids (mapv - (range 1 (inc n)))
        all-ids (into (vec existing-ids) new-ids)
        positions (cards/shuffle-positions all-ids)
        new-cards (for [id new-ids]
                    (cond-> {:db/id id :card/rank kind :card/location :draw :card/position (get positions id)}
                      effect (assoc :card/effect effect)
                      amount (assoc :card/effect-amount amount)
                      temporary? (assoc :card/temporary? true)))
        reposition (for [id existing-ids] {:db/id id :card/position (get positions id)})]
    (concat new-cards reposition [{:db/id (:db/id deck) :deck/cards new-ids}])))

(defmethod
  ^{:doc "Adds `n` fresh copies of `kind` to `deck-id`'s draw pile,
          reshuffling the draw pile's positions afterward so the new
          cards land somewhere random rather than always on top -- the
          generic perk/item deck-edit primitive ('add two +1 cards').
          Always a PLAIN card (no attached effect) -- see :attack-deck/
          add-effect-cards for cards carrying one. `temporary?` marks
          the new cards scenario-scoped (an item/scenario-effect grant,
          e.g. 'add one -1 card' from an item -- see :attack-deck/
          end-scenario); a permanent perk edit passes false."}
  event-tx-fn :attack-deck/add-cards
  [data _ deck-id kind n temporary?]
  (let [user (ds/entity data [:db/ident :user])
        deck (ds/entity data deck-id)]
    (if (or (<= n 0) (not (attack-deck-authorized? data user deck)))
      []
      (attack-deck-new-cards-tx deck kind n nil nil temporary?))))

(defmethod
  ^{:doc "Adds `n` fresh copies of `kind` carrying an attached `effect`
          (one of ogres.app.attack-deck/effect-kinds, e.g. :push with
          `amount` 2) to deck-id's draw pile -- the generic perk/item
          deck-edit primitive for the majority of real class perks,
          which add a card that does more than a plain ±N ('add three
          PUSH 1 cards', 'add one STUN card'). `amount` is only stored
          for effects whose :amount? is true, ignored otherwise.
          `temporary?` marks the new cards scenario-scoped, same as
          :attack-deck/add-cards. A no-op if `effect` isn't a recognized
          kind."}
  event-tx-fn :attack-deck/add-effect-cards
  [data _ deck-id kind effect amount n temporary?]
  (let [user (ds/entity data [:db/ident :user])
        deck (ds/entity data deck-id)
        effect-def (get attack-deck/effect-kinds effect)]
    (if (or (<= n 0) (nil? effect-def) (not (attack-deck-authorized? data user deck)))
      []
      (attack-deck-new-cards-tx deck kind n effect (if (:amount? effect-def) amount) temporary?))))

(defmethod
  ^{:doc "Removes up to `n` PLAIN copies of `kind` from `deck-id`
          (searched across both draw and discard, since these decks have
          no hand concept) -- the generic perk/item deck-edit primitive
          ('remove two -1 cards'). Only matches cards with no attached
          :card/effect -- an effect card sharing the same base kind is
          never accidentally consumed by a plain composition edit;
          removing one requires the explicit :attack-deck/remove-effect-
          cards. Removes as many matching cards as actually exist, up to
          `n` -- not an error if fewer are found."}
  event-tx-fn :attack-deck/remove-cards
  [data _ deck-id kind n]
  (let [user (ds/entity data [:db/ident :user])
        deck (ds/entity data deck-id)]
    (if-not (attack-deck-authorized? data user deck)
      []
      (let [targets (take n (filter (fn [c] (and (= (:card/rank c) kind) (nil? (:card/effect c))))
                                     (:deck/cards deck)))]
        (mapv (fn [c] [:db/retractEntity (:db/id c)]) targets)))))

(defmethod
  ^{:doc "Removes up to `n` cards matching `kind`, `effect`, AND `amount`
          exactly from `deck-id` (searched across both draw and discard)
          -- the effect-card counterpart to :attack-deck/remove-cards.
          Removes as many matching cards as actually exist, up to `n`."}
  event-tx-fn :attack-deck/remove-effect-cards
  [data _ deck-id kind effect amount n]
  (let [user (ds/entity data [:db/ident :user])
        deck (ds/entity data deck-id)]
    (if-not (attack-deck-authorized? data user deck)
      []
      (let [targets (take n (filter (fn [c] (and (= (:card/rank c) kind)
                                                  (= (:card/effect c) effect)
                                                  (= (:card/effect-amount c) amount)))
                                     (:deck/cards deck)))]
        (mapv (fn [c] [:db/retractEntity (:db/id c)]) targets)))))

(defmethod
  ^{:doc "Removes one `from-kind` card and adds one `to-kind` card in its
          place -- the generic perk/item deck-edit primitive ('replace
          one -2 card with one -1 card'). The replacement is always a
          PERMANENT card (perk edits, unlike item/scenario grants, don't
          expire -- see :attack-deck/add-cards' own `temporary?`). A
          no-op if `deck-id` has no `from-kind` card to remove."}
  event-tx-fn :attack-deck/replace-card
  [data _ deck-id from-kind to-kind]
  (let [deck (ds/entity data deck-id)]
    (if (empty? (filter (fn [c] (and (= (:card/rank c) from-kind) (nil? (:card/effect c))))
                         (:deck/cards deck)))
      []
      [[:db.fn/call event-tx-fn :attack-deck/remove-cards deck-id from-kind 1]
       [:db.fn/call event-tx-fn :attack-deck/add-cards deck-id to-kind 1 false]])))

(defn ^:private attack-deck-kind-count
  [deck kind]
  (count (filter (comp #{kind} :card/rank) (:deck/cards deck))))

(defn ^:private attack-deck-all-decks
  "Every attack modifier deck currently in play -- every roster player's
   own :player/attack-deck (root-scoped, wherever they are) plus the
   CURRENT scene's own :scene/monster-attack-deck, if it has one. The
   shared 'sweep everything' set both attack-deck-curse-pool-total and
   the scene-wide :attack-deck/reshuffle-flagged/end-scenario actions
   use -- unconditional (no 'is this player in the current scenario'
   concept exists or is needed: sweeping an inactive player's untouched
   deck is a harmless no-op in the realistic single-campaign-per-save
   case)."
  [data]
  (let [root (ds/entity data [:db/ident :root])
        scene (:camera/scene (:user/camera (ds/entity data [:db/ident :user])))]
    (concat (keep :player/attack-deck (:root/players root))
            (if-let [m (:scene/monster-attack-deck scene)] [m] []))))

(defn ^:private attack-deck-curse-pool-total
  "The combined CURSE count across every PERSONAL (owned) attack modifier
   deck currently in play -- the shared pool of 10 available for
   distribution to players (rulebook errata p.55: 'The curse deck is
   split into two equal decks of 10 cards each. One deck is exclusively
   for putting curse cards into the players' attack modifier decks...'
   -- ALL player decks draw from that SAME 10, not 10 each; the monster
   deck has its own separate, independent pool of 10, checked directly
   via attack-deck-kind-count since only one such deck ever exists).
   Computed live from the decks themselves rather than tracked as a
   separate counter, so it can never drift out of sync with what's
   actually in play."
  [data]
  (let [root (ds/entity data [:db/ident :root])]
    (apply + (map #(attack-deck-kind-count % :curse) (keep :player/attack-deck (:root/players root))))))

(defmethod
  ^{:doc "Adds `n` BLESS cards to `deck-id` -- shorthand for :attack-deck/
          add-cards with :bless, always :card/temporary? true (BLESS is
          scenario-scoped -- see :attack-deck/end-scenario -- on top of
          its existing removed-on-draw behavior). Uncapped: this
          rulebook has no confirmed pool-size errata for BLESS the way
          it does for CURSE (see attack-deck-curse-pool-total) -- an
          earlier per-deck cap of 10 here was an unconfirmed assumption
          (symmetry with curse) and has been removed rather than
          enforcing a guessed number."}
  event-tx-fn :attack-deck/add-bless
  [_ _ deck-id n]
  [[:db.fn/call event-tx-fn :attack-deck/add-cards deck-id :bless n true]])

(defmethod
  ^{:doc "Adds `n` CURSE cards to `deck-id` -- shorthand for :attack-deck/
          add-cards with :curse, always :card/temporary? true (same
          scenario-scoped reasoning as add-bless). Capped by
          attack-deck-curse-pool-total: adding to a PERSONAL deck is
          rejected outright (never partially applied) if it would push
          the SHARED total across every player's deck combined past 10;
          adding to the shared monster deck instead checks that one
          deck's own count, its own separate pool of 10."}
  event-tx-fn :attack-deck/add-curse
  [data _ deck-id n]
  (let [deck (ds/entity data deck-id)
        ;; `some?`, not `seq`: :player/attack-deck is :db/isComponent, so
        ;; this reverse ref is the single owning Entity, never a
        ;; collection. `seq` did give the right answer, but only because
        ;; seq-ing an Entity yields its attribute pairs -- it read as a
        ;; non-empty-collection test, and "correcting" it to `first` would
        ;; have quietly made every personal deck share the monster deck's
        ;; separate pool.
        current (if (some? (:player/_attack-deck deck))
                  (attack-deck-curse-pool-total data)
                  (attack-deck-kind-count deck :curse))]
    (if (> (+ current n) 10)
      []
      [[:db.fn/call event-tx-fn :attack-deck/add-cards deck-id :curse n true]])))

(defmethod
  ^{:doc "Reshuffles the whole of `deck-id` (draw + discard together) into
          a freshly-shuffled draw pile and clears :deck/needs-reshuffle?
          -- a manual 'reshuffle now' action, and what :attack-deck/
          reshuffle-flagged calls per flagged deck."}
  event-tx-fn :attack-deck/reshuffle
  [data _ deck-id]
  (let [user (ds/entity data [:db/ident :user])
        deck (ds/entity data deck-id)]
    (if-not (attack-deck-authorized? data user deck)
      []
      (let [ids (map :db/id (:deck/cards deck))
            positions (cards/shuffle-positions ids)]
        (conj (vec (for [id ids] {:db/id id :card/location :draw :card/position (get positions id)}))
              {:db/id deck-id :deck/needs-reshuffle? false})))))

(defmethod
  ^{:doc "Host-only: reshuffles every attack modifier deck currently in
          play (see attack-deck-all-decks -- every player's own deck
          plus the current scene's monster deck) that's currently
          flagged :deck/needs-reshuffle? (see :attack-deck/draw) in one
          sweep -- the manual 'end of round' action substituting for
          automatically hooking the generic :initiative/next event,
          which would otherwise need to know this specific game module
          exists."}
  event-tx-fn :attack-deck/reshuffle-flagged
  [data _]
  (let [user (ds/entity data [:db/ident :user])]
    (if-not (:user/host user)
      []
      (let [flagged (filter :deck/needs-reshuffle? (attack-deck-all-decks data))]
        (apply concat
               (for [deck flagged]
                 [[:db.fn/call event-tx-fn :attack-deck/reshuffle (:db/id deck)]]))))))

(defmethod
  ^{:doc "Host-only: removes every card still flagged :card/temporary?
          true from EVERY attack modifier deck currently in play (see
          attack-deck-all-decks), drawn-or-not -- BLESS/CURSE cards that
          were never drawn, plus any item/scenario-added plain or effect
          card (see :attack-deck/add-cards/add-effect-cards' own
          `temporary?` and add-bless/add-curse, which always set it).
          The FAQ (p.78) is explicit these 'should be removed from your
          deck at the end of a scenario' -- applied in one sweep, the
          same shape :attack-deck/reshuffle-flagged already established
          for its own 'sweep every deck currently in play' action."}
  event-tx-fn :attack-deck/end-scenario
  [data _]
  (let [user (ds/entity data [:db/ident :user])]
    (if-not (:user/host user)
      []
      (let [targets (mapcat (fn [deck] (filter :card/temporary? (:deck/cards deck)))
                             (attack-deck-all-decks data))]
        (mapv (fn [c] [:db/retractEntity (:db/id c)]) targets)))))

(defmethod
  ^{:doc "Resets `deck-id` all the way back to a fresh
          ogres.app.attack-deck/standard-composition, discarding every
          perk/item edit made to it -- a 'start the campaign over' action,
          unlike :attack-deck/reshuffle (which keeps the current
          composition, just re-pools it)."}
  event-tx-fn :attack-deck/reset
  [data _ deck-id]
  (let [user (ds/entity data [:db/ident :user])
        deck (ds/entity data deck-id)]
    (if-not (attack-deck-authorized? data user deck)
      []
      (let [old-ids (map :db/id (:deck/cards deck))
            [new-ids card-tx] (attack-deck-composition-tx)]
        (concat (map (fn [id] [:db/retractEntity id]) old-ids)
                card-tx
                [{:db/id deck-id :deck/cards new-ids :deck/needs-reshuffle? false}])))))

(defmethod
  ^{:doc "Removes the given attack modifier deck and all its cards
          (isComponent cleanup, same as :deck/remove)."}
  event-tx-fn :attack-deck/remove
  [data _ deck-id]
  (let [user (ds/entity data [:db/ident :user])
        deck (ds/entity data deck-id)]
    ;; The most destructive action in this family and, until now, the
    ;; only one not checked here -- :player/attack-deck is
    ;; :db/isComponent, so retracting the deck takes every card with it,
    ;; including a campaign's worth of perk edits. The panel button was
    ;; already :disabled for the unauthorized, but the UI is not the
    ;; boundary: any participant can dispatch this directly.
    (if-not (attack-deck-authorized? data user deck)
      []
      [[:db/retractEntity deck-id]])))

;; --- Dice ---
;; A generic n-sided-die primitive, independent of any game-type -- see
;; ogres.app.dice for the pure roll/combine logic. One roll (however
;; many dice were in the pool) is one entity in :scene/dice-rolls,
;; mirroring :scene/decks' own component/cardinality-many shape. D&D 5e
;; doesn't get a separate roller: its own :dnd5e/dice-roller element
;; (see game_type/games/dnd5e.cljs) just unlocks two extra, optional
;; arguments to this SAME event -- :roll/mode (advantage/disadvantage)
;; and :roll/owner (a specific roster player instead of a shared/
;; neutral roll) -- checked live against :game-type/enabled-elements,
;; the same pattern :go-fish/ask's ask-anyone? and :rummy/score-run's
;; runs-check already use for their own optional rules.

(defn ^:private dice-roll-tx
  "Tx-data (paired with the roll's own :db/id, as [roll-id tx-data])
   creating one new roll entity from `sides-seq` (e.g. [20 6 6] for
   1d20 + 2d6) -- the raw per-die results plus a single precomputed
   headline number, so a caller never needs to re-derive it: :roll/
   result is the sum of every die (`mode` nil, a plain multi-die pool)
   or the single best/worst value among them (`mode` :advantage/
   :disadvantage -- roll extra dice, take one, discard the rest).
   `owner-id`, when given, is a roster player this roll belongs to;
   nil leaves it a shared/neutral roll anyone can see, the default for
   the bare universal primitive."
  [sides-seq mode owner-id]
  (let [rolled (dice/roll-dice sides-seq)
        result (case mode
                 :advantage (:value (dice/best rolled))
                 :disadvantage (:value (dice/worst rolled))
                 (dice/sum rolled))
        roll-id -1]
    [roll-id
     [(cond-> {:db/id roll-id
               :roll/dice rolled
               :roll/result result
               :roll/rolled-at (.now js/Date)}
        mode (assoc :roll/mode mode)
        owner-id (assoc :roll/owner owner-id))]]))

(defmethod
  ^{:doc "Rolls `sides-seq` (a seq of side-counts, any positive integers
          -- the primitive itself isn't limited to the 7 standard D&D
          sizes, only the panel's own picker is) as one new entry in
          :scene/dice-rolls. A no-op if `sides-seq` is empty -- nothing
          to roll.

          `mode` (:advantage/:disadvantage/nil) and `owner-id` (a
          roster player, or nil for a shared/neutral roll) are only
          honored if :dnd5e/dice-roller is actually enabled on the
          scene's own game-type (checked here server-side, not just
          gated in the UI, since any connected participant may
          dispatch this) -- otherwise silently downgraded to nil/nil
          rather than rejecting the whole roll outright, so a stale
          client is never left unable to roll at all. When `owner-id`
          IS honored, the whole dispatch is rejected unless the viewer
          has player/authority? over that specific roster player --
          the same explicit-target-id-plus-authority-check shape :go-
          fish/score/:rummy/score already use. A neutral roll (no
          owner) needs no authority at all -- anyone may roll a shared
          die, the same 'anyone may start a mini-game table' spirit
          :old-maid/start's own docstring describes."}
  event-tx-fn :dice/roll
  [data _ sides-seq mode owner-id]
  (if (seq sides-seq)
    (let [user (ds/entity data [:db/ident :user])
          scene (:camera/scene (:user/camera user))
          enabled (:game-type/enabled-elements (:scene/game-type scene))
          dnd-dice? (contains? enabled :dnd5e/dice-roller)
          mode (if dnd-dice? mode)
          owner-id (if dnd-dice? owner-id)]
      (if (and owner-id
               (let [connected (into #{} (map :user/uuid) (:session/conns (ds/entity data [:db/ident :session])))
                     controller-uuid (get-in (ds/entity data owner-id) [:player/controller :user/uuid])]
                 (not (player/authority? (:user/uuid user) (:user/host user) connected controller-uuid))))
        []
        (let [[roll-id tx] (dice-roll-tx sides-seq mode owner-id)]
          (conj (vec tx) [:db.fn/call assoc-scene :scene/dice-rolls roll-id]))))
    []))

(defmethod
  ^{:doc "Clears the current scene's entire dice-roll history -- every
          entity in :scene/dice-rolls is retracted individually
          (isComponent means there's no single statement that clears a
          cardinality-many ref collection wholesale the way there is
          for a plain scalar attribute; :deck/remove's own
          :db/retractEntity is the same precedent). Host-only in the
          UI -- not enforced here, the same trust-the-UI convention
          every other admin-flavored action in this app already
          follows."}
  event-tx-fn :dice/clear
  [data _]
  (let [scene (:camera/scene (:user/camera (ds/entity data [:db/ident :user])))]
    (mapv (fn [roll] [:db/retractEntity (:db/id roll)]) (:scene/dice-rolls scene))))

;; --- Mini-game sessions ---
;; Generic scaffolding for nested, opt-in mini-game sessions: an
;; independent, simultaneously-runnable instance of a game (so far
;; just Old Maid -- see ogres.app.component.panel-old-maid) seated by
;; an arbitrary SUBSET of the roster, not "every active player" the
;; way every other example game's singleton :scene/<game>-* attributes
;; still assume. Mirrors :scene/decks' component/cardinality-many
;; shape rather than a singleton -- see :scene/minigames in
;; provider/state.cljs's schema. A session's participants are seat
;; sub-entities (:minigame/seats), not a flat vector of raw player
;; ids -- seating order needs an order, and a per-seat controller
;; override (see minigame-controller-uuid) needs somewhere of its own
;; to live rather than one shared, concurrently-editable map.

(defn ^:private minigame-label
  "An auto-numbered label for a new session of `kind` -- 'Old Maid 1',
   'Old Maid 2', etc, one past however many sessions of that same kind
   already exist among `existing-minigames` (a scene's :scene/
   minigames). Not reused after a session ends, so labels stay unique
   for the scene's lifetime even if that leaves gaps."
  [existing-minigames kind title]
  (let [existing (filter (comp #{kind} :minigame/kind) existing-minigames)]
    (str title " " (inc (count existing)))))

(defn ^:private minigame-seats
  "`minigame`'s seats, in seating order."
  [minigame]
  (sort-by :seat/order (:minigame/seats minigame)))

(defn ^:private minigame-controller-uuid
  "The effective controller uuid for `seat` -- its own per-session
   :seat/controller override if assigned, else its roster player's
   ordinary :player/controller, else nil (host fallback). Feeds
   ogres.app.player/authority? exactly like every other game's
   controller-uuid resolution, just with this extra per-session layer
   in front of the roster's own -- an NPC (or any seat) can be handed
   to any connected participant for the duration of one table without
   touching the roster's own persistent controller assignment."
  [seat]
  (or (get-in seat [:seat/controller :user/uuid])
      (get-in seat [:seat/player :player/controller :user/uuid])))

(defn ^:private minigame-create-tx
  "Tx-data creating a new mini-game session's seats (one ref per
   participant, in order) and the session entity itself, at the
   caller's own already-reserved `minigame-id` -- the caller is
   responsible for picking an id that doesn't collide with whatever
   ELSE it's creating in the same transaction (e.g. `(dec deck-id)`
   for a deck-owning game, matching deck-create-tx's own negative-
   temp-id idiom, or a step past however many card/prop ids a dealt-
   from-scratch game like Memory already consumed). `deck-id`, when
   given, is an ALREADY-CREATED deck (typically deck-create-tx's own
   result, composed by the caller the same way :go-fish/start composes
   deck-create-tx with its own deal logic) -- deliberately NOT also
   added to :scene/decks, so the session's own :db/retractEntity
   cascades deck+cards for free later (see :minigame/remove), and
   in-play game decks don't clutter the Decks panel while a table is
   running. `deck-id` is nil for a game with no deck at all (e.g.
   Memory, whose 'cards' are plain scene props tracked via :minigame/
   props instead -- see minigame-props-cleanup-tx)."
  [minigame-id kind label deck-id participant-ids]
  (let [seats (map-indexed
               (fn [i player-id] {:db/id (- minigame-id 1 i) :seat/player player-id :seat/order i})
               participant-ids)]
    (concat
     seats
     [(cond-> {:db/id minigame-id
               :minigame/kind kind
               :minigame/label label
               :minigame/seats (mapv :db/id seats)
               :minigame/turn-index 0
               :minigame/neutral-authority? true}
        deck-id (assoc :minigame/deck deck-id))])))

(defn ^:private minigame-attach-tx
  "Tx-data attaching a freshly-created session to the current scene's
   :scene/minigames and pointing the creator's own :user/minigame-
   viewing at it -- the shared last step every game's own /start
   shares, after minigame-create-tx (and, for a deck-owning game,
   whatever dealing tx-data) has already been assembled."
  [scene-id minigame-id]
  [{:db/id scene-id :scene/minigames minigame-id}
   {:db/ident :user :user/minigame-viewing minigame-id}])

(defmethod
  ^{:doc "Assigns (or, given a nil uuid, clears) a connected
          participant's per-session control over `player-id`'s seat
          within `minigame-id` -- an override on top of that roster
          player's ordinary :player/controller, scoped to just this
          one session (see minigame-controller-uuid). A no-op if
          `player-id` isn't actually seated at this session."}
  event-tx-fn :minigame/set-controller
  [data _ minigame-id player-id uuid]
  (let [user (ds/entity data [:db/ident :user])
        minigame (ds/entity data minigame-id)
        seat (first (filter (comp #{player-id} :db/id :seat/player) (:minigame/seats minigame)))]
    ;; Host-only, like the roster's own controller-picker this is the
    ;; per-session sibling of (see component/panel's `visible-tabs` --
    ;; the Players tab is host-only because seating is a GM concern).
    ;; :seat/controller is the FIRST branch of minigame-controller-uuid,
    ;; so whoever writes it decides who may act on that seat and who
    ;; sees its hand face-up -- without this check any guest could point
    ;; another player's seat at themselves and read their hand.
    (if (and seat (:user/host user))
      (if uuid
        [{:db/id (:db/id seat) :seat/controller [:user/uuid uuid]}]
        [[:db/retract (:db/id seat) :seat/controller]])
      [])))

(defmethod
  ^{:doc "Removes the given mini-game session entirely -- its seats and
          its owned deck (and every card in it), if it has one, all
          cascade via :db/isComponent, the same one-retraction cleanup
          :deck/remove already relies on. Memory's cards cascade the
          same way via :minigame/cards -- they used to be borrowed
          scene props that had to be retracted by hand here, which is
          exactly the coupling that let main-game actions reach into a
          running table. Any participant, not just the host, may end
          their own table -- mirrors how anyone may start one (see
          :old-maid/start)."}
  event-tx-fn :minigame/remove
  [_ _ minigame-id]
  [[:db/retractEntity minigame-id]])

(defmethod
  ^{:doc "Sets which of the scene's mini-game sessions the local user is
          currently looking at -- mirrors :user/edit-game-type, just
          for the session list/detail view instead of the game-type
          builder's."}
  event-tx-fn :user/view-minigame
  [_ _ minigame-id]
  [{:db/ident :user :user/minigame-viewing minigame-id}])

;; --- Players ---
(defmethod
  ^{:doc "Creates a new player or NPC (kind is :human or :npc) on the
          global roster (:root/players, shared across every scene). Name
          is auto-assigned as 'Player N'/'NPC N' (N one more than however
          many same-kind participants already exist, so numbering is
          per-kind); color is auto-assigned as the first color from the
          kind's own palette (ogres.app.player/colors-for-kind -- humans
          and NPCs draw from two disjoint palettes, see player.cljs) not
          already used by another participant of the same kind, active or
          benched (a benched player's color stays reserved), via
          ogres.app.player/next-color -- the same
          first-available-color-at-connect-time idea
          provider.session/next-color already uses for connected users'
          cursor colors, just applied to a persistent roster instead."}
  event-tx-fn :player/create
  [data _ kind]
  (let [root (ds/entity data [:db/ident :root])
        existing (:root/players root)
        same-kind (filter (comp #{kind} :player/kind) existing)
        taken (into #{} (map :player/color) same-kind)]
    [{:db/ident :root
      :root/players
      [{:player/name (str (if (= kind :npc) "NPC" "Player") " " (inc (count same-kind)))
        :player/kind kind
        :player/color (player/next-color (player/colors-for-kind kind) taken)
        :player/active true}]}]))

(defmethod
  ^{:doc "Renames the given player/NPC."}
  event-tx-fn :player/rename
  [_ _ player-id name]
  [{:db/id player-id :player/name name}])

(defmethod
  ^{:doc "Sets the given player/NPC's color, unless another participant of
          the same kind on the roster (active or benched) already has it
          -- a no-op in that case. Humans and NPCs draw from disjoint
          palettes (ogres.app.player/colors-for-kind), so only same-kind
          participants can ever conflict. The roster UI also disables
          already-taken swatches, but this enforces the color-exclusivity
          invariant regardless of caller, the same spirit as
          :game-type/toggle-element's :exclusive-group handling."}
  event-tx-fn :player/change-color
  [data _ player-id color]
  (let [entity (ds/entity data player-id)
        root (ds/entity data [:db/ident :root])
        same-kind (filter (comp #{(:player/kind entity)} :player/kind) (:root/players root))
        taken-by-other
        (some #(and (= (:player/color %) color) (not= (:db/id %) player-id)) same-kind)]
    (if taken-by-other [] [{:db/id player-id :player/color color}])))

(defmethod
  ^{:doc "Sets the given player/NPC's kind (:human or :npc). Humans and
          NPCs draw from disjoint color palettes
          (ogres.app.player/colors-for-kind), so a kind change also
          reassigns color to the first available color in the new kind's
          palette (same reservation rule as :player/create, scanning
          active and benched participants of the new kind) -- the old
          color would otherwise be meaningless (or, worse, collide) in
          the new palette. A no-op if `kind` matches the current kind."}
  event-tx-fn :player/change-kind
  [data _ player-id kind]
  (let [entity (ds/entity data player-id)]
    (if (= kind (:player/kind entity))
      []
      (let [root (ds/entity data [:db/ident :root])
            same-kind (filter (comp #{kind} :player/kind) (:root/players root))
            taken (into #{} (map :player/color) same-kind)]
        [{:db/id player-id
          :player/kind kind
          :player/color (player/next-color (player/colors-for-kind kind) taken)}]))))

(defmethod
  ^{:doc "Sets which currently-connected user controls the given
          player/NPC (a ref to their :user/uuid), or clears it (control
          reverts to the host default) when user-id is nil. This is how
          NPC control (host by default) gets reassigned to a guest, and
          how a human roster entry gets bound to whichever guest is
          playing them -- see ogres.app.player/authority?, which
          resolves this against currently-connected sessions at read
          time (a stale controller whose session disconnected is
          treated as unassigned, not an error)."}
  event-tx-fn :player/set-controller
  [data _ player-id user-id]
  ;; Host-only. The Players tab that dispatches this is already
  ;; host-only, so this changes no legitimate flow -- but :player/
  ;; controller is precisely what player/authority? keys on, so an
  ;; unchecked write here hands the writer authority over that player's
  ;; objects, attack deck and character profile. Same reasoning as
  ;; :minigame/set-controller's own check.
  (if (:user/host (ds/entity data [:db/ident :user]))
    (if user-id
      [{:db/id player-id :player/controller user-id}]
      [[:db/retract player-id :player/controller]])
    []))

(defmethod
  ^{:doc "Takes the given player/NPC out of active play (active? false) or
          restores them (active? true), without deleting the roster
          entry. Their name, kind, and color are untouched and the color
          stays reserved either way -- this is the reversible 'bench/
          restore' action, distinct from the permanent :player/remove."}
  event-tx-fn :player/set-active
  [_ _ player-id active?]
  [{:db/id player-id :player/active active?}])

(defmethod
  ^{:doc "Removes the given player/NPC permanently."}
  event-tx-fn :player/remove
  [_ _ player-id]
  [[:db/retractEntity player-id]])

;; --- Character Profile ---
;; Generic level/experience/gold/items tracking directly on the roster
;; player entity -- see :tool/character-profile (game_type/core_
;; elements.cljs). Deliberately NOT Gloomhaven-specific: any game-type
;; with leveling/currency (D&D included) can opt in the same way
;; Gloomhaven's own seeded template does. :root/players is already
;; root-scoped (not scene-scoped) and already sits inside this app's
;; automatic whole-DB save (provider/idb.cljs) -- these fields persist
;; across scene changes and browser reloads for free, no new persistence
;; plumbing, just new attributes on an entity that already survives
;; everything. Plain editable numbers and a flat named-item list -- no
;; XP-threshold table or item slot/equip-restriction rules are enforced,
;; the same 'trust the humans, don't automate rules resolution' stance
;; the rest of this app already takes everywhere else.

(defn ^:private character-profile-authorized?
  "Whether `user` may edit `player-id`'s character profile -- the
   controlling player (if connected), or the host -- the same player/
   authority? check :attack-deck/*'s own owner-authorization already
   uses, letting a player manage their own character (level up, track
   gold, add items) without host intervention, unlike the base Roster
   tab's own host-only admin actions."
  [data user player-id]
  (let [connected (into #{} (map :user/uuid) (:session/conns (ds/entity data [:db/ident :session])))
        controller-uuid (get-in (ds/entity data player-id) [:player/controller :user/uuid])]
    (player/authority? (:user/uuid user) (:user/host user) connected controller-uuid)))

(defmethod
  ^{:doc "Sets the given player/NPC's level. A plain editable number --
          no XP-threshold table is enforced."}
  event-tx-fn :player/set-level
  [data _ player-id value]
  (if (character-profile-authorized? data (ds/entity data [:db/ident :user]) player-id)
    [{:db/id player-id :player/level value}]
    []))

(defmethod
  ^{:doc "Sets the given player/NPC's experience total. A plain editable
          number, same reasoning as :player/set-level."}
  event-tx-fn :player/set-experience
  [data _ player-id value]
  (if (character-profile-authorized? data (ds/entity data [:db/ident :user]) player-id)
    [{:db/id player-id :player/experience value}]
    []))

(defmethod
  ^{:doc "Sets the given player/NPC's gold/currency total."}
  event-tx-fn :player/set-gold
  [data _ player-id value]
  (if (character-profile-authorized? data (ds/entity data [:db/ident :user]) player-id)
    [{:db/id player-id :player/gold value}]
    []))

(defmethod
  ^{:doc "Adds one named item (with an optional description) to the given
          player/NPC's inventory, unequipped by default -- a generic
          'this character owns a thing' primitive, no slot/weight-limit
          rules enforced (the same no-reinvented-rules-resolution stance
          the attack-deck deck-edit primitives already take)."}
  event-tx-fn :player/add-item
  [data _ player-id name description]
  (if (character-profile-authorized? data (ds/entity data [:db/ident :user]) player-id)
    [{:db/id player-id
      :player/items [(cond-> {:item/name (str name)}
                       (seq description) (assoc :item/description (str description)))]}]
    []))

(defmethod
  ^{:doc "Toggles whether the given item is currently equipped."}
  event-tx-fn :player/toggle-item-equipped
  [data _ item-id value]
  (let [owner (:player/_items (ds/entity data item-id))]
    (if (and owner (character-profile-authorized? data (ds/entity data [:db/ident :user]) (:db/id owner)))
      [{:db/id item-id :item/equipped? value}]
      [])))

(defmethod
  ^{:doc "Removes the given item from its owner's inventory entirely."}
  event-tx-fn :player/remove-item
  [data _ item-id]
  (let [owner (:player/_items (ds/entity data item-id))]
    (if (and owner (character-profile-authorized? data (ds/entity data [:db/ident :user]) (:db/id owner)))
      [[:db/retractEntity item-id]]
      [])))

(defn ^:private import-character-keyword
  "Untrusted value -> keyword, nil for anything that is neither a keyword
   nor a string. (keyword 42) happens to evaluate to nil under
   ClojureScript today, but leaning on that is how a bad blob ends up
   transacting a nil value; be explicit instead."
  [x]
  (cond (keyword? x) x
        (string? x)  (keyword x)))

(defn ^:private import-character-text
  "Untrusted value -> trimmed string, \"\" for anything non-string. Not
   `str`, which happily renders nil as \"\" and a map as its literal
   source -- both of which then show up verbatim in the panel."
  [x]
  (if (string? x) (trim x) ""))

(defn ^:private import-character-card
  "One untrusted card entry -> card tx data, or nil to drop it. A card is
   kept only if it names one of attack-deck's 9 kinds -- `value` returns
   nil for anything else, the same recognised-or-no-op test
   :attack-deck/add-effect-cards already applies to its own `effect`
   argument. An unrecognised effect is dropped while keeping the card,
   and an amount is kept only for the effects that take one, exactly as
   that event does. :db/id and :card/position are assigned by the caller
   once the surviving cards are known."
  [entry]
  (if (map? entry)
    (let [kind (import-character-keyword (:rank entry))
          effect (import-character-keyword (:effect entry))
          effect-def (get attack-deck/effect-kinds effect)
          amount (:amount entry)]
      (if (some? (attack-deck/value kind))
        (cond-> {:card/rank kind :card/location :draw}
          (some? effect-def) (assoc :card/effect effect)
          (and (:amount? effect-def) (number? amount)) (assoc :card/effect-amount amount))))))

(defmethod
  ^{:doc "Creates a brand-new player (kind :human) from a previously-
          exported character blob (see component/panel_character.cljs's
          export-character!) -- name, level, experience, gold, items,
          and a full attack modifier deck composition (draw/discard
          split is NOT carried over -- an imported character always
          starts fresh-shuffled, same as a newly created one). Mirrors
          :game-type/import's own shape and trust level: no authority
          check (matches :player/create's own trust-the-UI convention
          for roster-admin actions), plain type coercion on the
          untrusted input, everything built in one transaction. Not
          gated on :gloomhaven/attack-deck being enabled -- the
          character is still created without a deck-carrying import if
          `deck` is empty, or even if the importing scene's game-type
          doesn't have the element on (just without a tab to view it in
          until it does)."}
  event-tx-fn :player/import-character
  [data _ {:keys [name level experience gold items deck]}]
  (let [root (ds/entity data [:db/ident :root])
        taken (into #{} (map :player/color) (:root/players root))
        player-id -1
        deck-id -2
        ;; Everything below treats the blob as hostile. It arrives from a
        ;; file the user picked, so it can be truncated, hand-edited, or
        ;; written by an older release -- a bad one must yield a poorer
        ;; character, never a failed transaction.
        cards-in (if (sequential? deck) deck [])
        items-in (if (sequential? items) items [])
        coerced (keep import-character-card cards-in)
        card-ids (mapv - (range 3 (+ 3 (count coerced))))
        positions (cards/shuffle-positions card-ids)
        card-tx (map (fn [c id] (assoc c :db/id id :card/position (get positions id)))
                     coerced card-ids)
        item-tx (into []
                      (keep (fn [entry]
                              (if (map? entry)
                                (let [nm (import-character-text (:name entry))
                                      ds (import-character-text (:description entry))]
                                  ;; a nameless item is just a blank row in
                                  ;; the panel -- drop it rather than show it
                                  (if (seq nm)
                                    (cond-> {:item/name nm}
                                      (seq ds) (assoc :item/description ds)
                                      (true? (:equipped? entry)) (assoc :item/equipped? true)))))))
                      items-in)
        player-name (let [n (import-character-text name)]
                      (if (seq n) n "Imported Character"))]
    (concat
     [(cond-> {:db/id player-id
               :player/name player-name
               :player/kind :human
               :player/color (player/next-color (player/colors-for-kind :human) taken)
               :player/active true}
        (seq item-tx) (assoc :player/items item-tx)
        (number? level) (assoc :player/level level)
        (number? experience) (assoc :player/experience experience)
        (number? gold) (assoc :player/gold gold))]
     (if (seq card-tx)
       (concat [{:db/id deck-id :deck/name player-name :deck/cards card-ids}]
               card-tx
               [[:db/add player-id :player/attack-deck deck-id]])
       [])
     [[:db/add [:db/ident :root] :root/players player-id]])))

;; --- Token Images ---
(defmethod event-tx-fn :token-images/create-many
  ([_ event images]
   [[:db.fn/call event-tx-fn event images false]])
  ([_ _ images public?]
   (into [{:db/ident :root
           :root/token-images
           (for [[{:keys [hash name size width height]} _] images]
             {:image/hash hash
              :image/name name
              :image/size size
              :image/public public?
              :image/width width
              :image/height height})}] cat
         (for [[image thumbnail] images]
           (if (= (:hash image) (:hash thumbnail))
             [{:image/hash (:hash image) :image/thumbnail [:image/hash (:hash image)]}]
             [{:image/hash (:hash thumbnail)
               :image/name (:name thumbnail)
               :image/size (:size thumbnail)
               :image/width (:width thumbnail)
               :image/height (:height thumbnail)}
              {:image/hash (:hash image) :image/thumbnail [:image/hash (:hash thumbnail)]}])))))

(defmethod
  ^{:doc "Change the visibility of the given token image to public (true)
          or private (false)."}
  event-tx-fn :token-images/change-scope
  [_ _ hash public?]
  [[:db/add -1 :image/hash hash]
   [:db/add -1 :image/public public?]])

(defmethod event-tx-fn :token-images/remove
  [_ _ image thumb]
  (if (= image thumb)
    [[:db/retractEntity [:image/hash image]]]
    [[:db/retractEntity [:image/hash image]]
     [:db/retractEntity [:image/hash thumb]]]))

(defmethod event-tx-fn :token-images/remove-all
  []
  [[:db/retract [:db/ident :root] :root/token-images]])

(defmethod event-tx-fn :token-images/change-thumbnail
  [_ _ hash thumb rect rotation]
  [{:image/hash hash
    :image/thumbnail-rect rect
    ;; The rect is expressed in the rotated frame the editor was showing,
    ;; so the turn has to be stored with it -- reopening the editor without
    ;; it would restore the crop box against a differently-oriented image.
    :image/thumbnail-rotation (or rotation 0)
    :image/thumbnail
    {:image/hash (:hash thumb)
     :image/size (.-size (:data thumb))
     :image/width (:width thumb)
     :image/height (:height thumb)}}])

(defmethod
  ^{:doc ""}
  event-tx-fn
  :token-images/change-default-label
  [_ _ hash label]
  (let [value (trim label)]
    (if (= value "")
      [[:db/retract [:image/hash hash] :token-image/default-label]]
      [[:db/add [:image/hash hash] :token-image/default-label label]])))

(defmethod
  ^{:doc ""}
  event-tx-fn
  :token-images/change-url
  [_ _ hash url]
  (let [value (trim url)]
    (if (= value "")
      [[:db/retract [:image/hash hash] :token-image/url]]
      [[:db/add [:image/hash hash] :token-image/url value]])))

(defmethod
  ^{:doc ""}
  event-tx-fn
  :token-images/change-details
  [_ _ hash default-label url]
  [[:db.fn/call event-tx-fn :token-images/change-default-label hash default-label]
   [:db.fn/call event-tx-fn :token-images/change-url hash url]])

;; --- Masks ---
(defmethod
  ^{:doc "Sets the current scene to be entirely masked by default. This is
          useful when the scene image is composed of many rooms and mostly
          dead space between them, such as a dungeon, and it is more efficient
          to 'carve out' the scene instead of filling it in."}
  event-tx-fn :scene/mask
  []
  [[:db.fn/call assoc-scene :scene/masked true]])

(defmethod
  ^{:doc "Sets the current scene to not be entirely masked by default. This
          is the default behavior."}
  event-tx-fn :scene/reveal
  []
  [[:db.fn/call assoc-scene :scene/masked false]])

(defmethod
  ^{:doc "Creates a new mask object for the current scene, accepting its
          current state (hide or reveal) and its polygon points as a flat
          vector of x, y pairs."}
  event-tx-fn :mask/create
  [_ _ vecs]
  [[:db.fn/call assoc-scene :scene/masks {:mask/vecs vecs}]])

(defmethod
  ^{:doc "Toggles the state of the given mask to be either hiding or revealing
          its contents."}
  event-tx-fn :mask/toggle
  [_ _ id state]
  [{:db/id id :mask/enabled? state}])

(defmethod
  ^{:doc "Removes the given mask object."}
  event-tx-fn :mask/remove
  [_ _ id]
  [[:db/retractEntity id]])

;; -- Session --
(defmethod
  ^{:doc "Attempts to start a new online session through the server. This
          transaction only updates the connection status of the local user
          and expands the session panel form."}
  event-tx-fn :session/request
  []
  [{:db/ident :root :root/session
    {:db/ident :session :session/host
     {:db/ident :user :session/status :connecting :panel/selected :lobby}}}])

(defmethod
  ^{:doc "Attempts to join an existing online session through the server. This
          transaction only updates the connection status of the local user."}
  event-tx-fn :session/join
  []
  [{:db/ident :user :session/status :connecting}])

(defmethod
  ^{:doc "Destroys the existing online session, pruning it and all player
          user state."}
  event-tx-fn :session/close
  []
  [{:db/ident :user :session/status :disconnected}
   [:db/retract [:db/ident :session] :session/host]
   [:db/retract [:db/ident :session] :session/conns]])

(defmethod
  ^{:doc "Toggles whether or not live cursors are displayed for everyone
          in the online session."}
  event-tx-fn :session/toggle-share-cursors
  [_ _ enabled]
  [{:db/ident :session :session/share-cursors enabled}])

(defmethod event-tx-fn :session/change-status
  ^{:doc "Updates the user's session status in response to changes to
          the WebSocket's ready state."}
  [_ _ status]
  (let [statuses {0 :connecting 1 :connected 2 :disconnecting 3 :disconnected}]
    [{:db/ident :user :session/status (statuses status)}]))

(defmethod
  ^{:doc "Toggles whether or not the cursor of the local user is displayed
          to other players in the session."}
  event-tx-fn :session/toggle-share-my-cursor
  [_ _ enabled]
  [{:db/ident :user :user/share-cursor enabled}])

(defmethod
  ^{:doc "Updates the current scene, camera position, and zoom level of all
          players in the online session to match the host. This is useful to
          bring a new scene or encounter to attention."}
  event-tx-fn :session/focus
  [data]
  (let [select-w [:camera/scene [:camera/point :default vec/zero] [:camera/scale :default 1]]
        select-l [:db/id [:user/bounds :default seg/zero]
                  {:user/cameras [:camera/scene] :user/camera select-w}]
        select-s [{:session/host select-l} {:session/conns select-l}]
        result   (ds/pull data select-s [:db/ident :session])
        {{bounds :user/bounds
          {point :camera/point} :user/camera
          host :user/camera} :session/host
         conns :session/conns} result
        scale (:camera/scale host)
        center (vec/add point (vec/div (seg/midpoint bounds) scale))]
    (->> (for [[next conn] (sequence (indexed) conns)
               :let [prev (->> (:user/cameras conn)
                               (filter (fn [conn]
                                         (= (:db/id (:camera/scene conn))
                                            (:db/id (:camera/scene host))))) (first) (:db/id))
                     next  (or prev next)
                     point (vec/sub center (vec/div (seg/midpoint (:user/bounds conn)) scale))]]
           [{:db/id (:db/id conn) :user/camera next :user/cameras next}
            {:db/id next
             :camera/point point
             :camera/scale scale
             :camera/scene (:db/id (:camera/scene host))}])
         (into [] cat))))

;; -- Clipboard --
(def ^:private clipboard-copy-attrs
  [:object/point
   :object/type
   :object/hidden
   :object/locked
   :object/scale
   :object/rotation
   :object/rotation-mode
   :object/layer-shift
   :note/icon
   :note/label
   :note/description
   :shape/points
   :shape/color
   :shape/pattern
   :token/label
   :token/flags
   :token/light
   :token/size
   :token/aura-radius
   :token/image
   :prop/image
   :board/image])

(def ^:private clipboard-copy-select
  [{:user/camera
    [{:camera/selected
      (into clipboard-copy-attrs
            [:db/id
             {:prop/image [:image/hash :image/width :image/height :image/anchor]}
             {:board/image [:image/hash :image/width :image/height :image/anchor]}
             {:token/image [:image/hash]}])}]}])

(defmethod
  ^{:doc "Copy the currently selected objects to the clipboard. Optionally
          removes them from the current scene if cut? is passed as true.
          The clipboard contains a template for the object data and not
          references to the objects themselves since those references
          don't exist after they are pruned from the scene. Only some object
          data is copied; transient state is not preserved."}
  event-tx-fn :clipboard/copy
  ([_ event]
   [[:db.fn/call event-tx-fn event false]])
  ([data _ cut?]
   (let [result (ds/pull data clipboard-copy-select [:db/ident :user])
         copied (:camera/selected (:user/camera result))
         copies (into [] (map #(select-keys % clipboard-copy-attrs)) copied)]
     (cond-> []
       (seq copies) (conj {:db/ident :user :user/clipboard copies})
       cut?         (into (for [{id :db/id} copied]
                            [:db/retractEntity id]))))))

(def ^:private clipboard-paste-select
  [{:root/user
    [[:user/clipboard :default []]
     [:user/bounds :default seg/zero]
     {:user/camera
      [:db/id
       [:camera/scale :default 1]
       [:camera/point :default vec/zero]
       {:camera/scene
        [:db/id
         [:scene/grid-align :default false]
         [:scene/grid-type :default :square]
         {:scene/game-type
          [[:game-type/enabled-elements :default #{}]]}]}]}]}
   {:root/token-images [:image/hash]}
   {:root/props-images [:image/hash]}
   {:root/scene-images [:image/hash]}])

(defmethod
  ^{:doc "Creates objects on the current scene from the data stored in the
          local user's clipboard. Attempts to preserve the relative position
          of the objects when they were copied but in the center of the user's
          viewport. Clipboard data is not pruned after pasting."}
  event-tx-fn :clipboard/paste
  [data]
  (let [result (ds/pull data clipboard-paste-select [:db/ident :root])
        {{clipboard :user/clipboard
          screen :user/bounds
          {camera :db/id
           scale :camera/scale
           point :camera/point
           {scene :db/id align? :scene/grid-align grid-type :scene/grid-type
            game-type-entity :scene/game-type}
           :camera/scene} :user/camera} :root/user
         token-images :root/token-images
         props-images :root/props-images
         scene-images :root/scene-images} result
        align? (and align? (pos? (game-type/grid-count (:game-type/enabled-elements game-type-entity #{}))))
        base-type (geom/base-grid-type grid-type)
        props-hashes (into #{} (map :image/hash) props-images)
        token-hashes (into #{} (map :image/hash) token-images)
        board-hashes (into #{} (map :image/hash) scene-images)
        pastable-xf
        (filter
         (fn [data]
           (case (:object/type data)
             :prop/prop (props-hashes (:image/hash (:prop/image data)))
             :board/piece (board-hashes (:image/hash (:board/image data)))
             true)))
        bound (transduce (mapcat geom/object-bounding-rect) geom/bounding-rect-rf clipboard)
        delta (vec/sub
               (vec/add point (geom/screen->scene-vec (seg/midpoint screen) scale grid-type))
               (seg/midpoint bound))]
    (for [[idx copy] (sequence (comp pastable-xf (indexed)) clipboard)
          :let [{hash-prop :image/hash} (:prop/image copy)
                hash-board (:image/hash (:board/image copy))
                hash-token (:image/hash (:token/image copy))
                type (keyword (namespace (:object/type copy)))
                moved (assoc copy :db/id idx :object/point (vec/add (:object/point copy) delta))
                data (cond-> moved
                       align?
                       (assoc :object/point (vec/rnd (:object/point moved) grid-size))
                       (and (= type :prop) (props-hashes hash-prop))
                       (assoc :prop/image [:image/hash (props-hashes hash-prop)])
                       (and (= type :board) (board-hashes hash-board))
                       (assoc :board/image [:image/hash (board-hashes hash-board)])
                       (and (= type :token) (token-hashes hash-token))
                       (assoc :token/image [:image/hash (token-hashes hash-token)])
                       (and align? (contains? snap-to-cell-types (:object/type copy)))
                       (assoc :object/point (geom/snap-to-cell moved vec/zero base-type)))]]
      {:db/id camera
       :camera/selected idx
       :camera/scene
       (cond-> {:db/id scene}
         (= type :note)  (assoc :scene/notes data)
         (= type :prop)  (assoc :scene/props data)
         (= type :board) (assoc :scene/board data)
         (= type :shape) (assoc :scene/shapes data)
         (= type :token) (assoc :scene/tokens data))})))

;; -- Shortcuts --
(defmethod
  ^{:doc "Handles the 'Escape' keyboard shortcut, clearing any token
          selections and changing the mode to `select`."}
  event-tx-fn :shortcut/escape
  [data]
  (let [user (ds/entity data [:db/ident :user])
        id   (:db/id (:user/camera user))]
    [{:db/id id :camera/draw-mode :select}
     [:db/retract id :camera/selected]]))

;; -- Dragging --
(defmethod
  ^{:doc "User has started dragging a single object."}
  event-tx-fn :drag/start
  [_ _ id]
  [{:db/ident :user :user/dragging id}])

(defmethod
  ^{:doc "User has started dragging all selected objects."}
  event-tx-fn :drag/start-selected
  [data _]
  (let [result (ds/entity data [:db/ident :user])
        {{selected :camera/selected} :user/camera} result]
    [{:db/ident :user :user/dragging (map :db/id selected)}]))

(defmethod
  ^{:doc "User has ended all dragging."}
  event-tx-fn :drag/end
  []
  [[:db/retract [:db/ident :user] :user/dragging]])

;; -- Notes --
(defmethod
  ^{:doc "Create a new note object at the given point."}
  event-tx-fn :note/create
  [data _ point]
  (let [user (ds/entity data [:db/ident :user])
        {bounds :user/bounds
         {camera :db/id
          {scene :db/id grid-type :scene/grid-type} :camera/scene
          shift :camera/point
          scale :camera/scale} :user/camera} user]
    [{:db/id -1
      :object/type :note/note
      :object/point
      (-> (vec/sub point (.-a bounds))
          (geom/screen->scene-vec scale grid-type)
          (vec/add shift)
          (vec/shift -16)
          (vec/rnd))
      :object/hidden true
      :object/locked false}
     {:db/id scene :scene/notes -1}
     {:db/id camera :camera/selected -1 :camera/draw-mode :select}]))

(defmethod
  ^{:doc "Change the selected icon for the given note. icon-name is a
          string that corresponds to a unique icon name."}
  event-tx-fn :note/change-icon
  [_ _ id icon-name]
  [[:db/add id :note/icon icon-name]])

(defmethod
  ^{:doc "Change the label for the given note."}
  event-tx-fn :note/change-label
  [_ _ id value]
  [[:db/add id :note/label value]])

(defmethod
  ^{:doc "Change the description for the given note."}
  event-tx-fn :note/change-description
  [_ _ id value]
  [[:db/add id :note/description value]])

(defmethod
  ^{:doc "Change both the label and description for the given note
          and close the form for editing."}
  event-tx-fn :note/change-details
  [data _ id label desc]
  (let [user (ds/entity data [:db/ident :user])]
    [[:db/add id :note/label label]
     [:db/add id :note/description desc]
     [:db/retract (:db/id (:user/camera user)) :camera/selected id]]))

;; --- Props Images ---

(defmethod
  ^{:doc "Creates state representations of images used as props which
          include metadata like image dimensions, filename, and their
          thumbnails. Uniquely identified by their SHA-1 hash digest."}
  event-tx-fn :props-images/create-many
  [_ _ images]
  (into
   [{:db/ident :root
     :root/props-images
     (for [[{:keys [hash name size width height]} _] images]
       {:image/hash hash
        :image/name name
        :image/size size
        :image/width width
        :image/height height})}] cat
   (for [[image thumbnail] images]
     (if (= (:hash image) (:hash thumbnail))
       [{:image/hash (:hash image) :image/thumbnail [:image/hash (:hash image)]}]
       [{:image/hash (:hash thumbnail)
         :image/name (:name thumbnail)
         :image/size (:size thumbnail)
         :image/width (:width thumbnail)
         :image/height (:height thumbnail)}
        {:image/hash (:hash image) :image/thumbnail [:image/hash (:hash thumbnail)]}]))))

(defmethod
  ^{:doc "Removes all prop images as well as any props from all scenes."}
  event-tx-fn :props-images/remove-all
  [data _ _]
  (let [user (ds/entity data [:db/ident :user])
        xfrm (comp (map :camera/scene) (mapcat :scene/props) (map :db/id))]
    (into [[:db/retract [:db/ident :root] :root/props-images]]
          (for [id (sequence xfrm (:user/cameras user))]
            [:db/retractEntity id]))))

(defmethod
  ^{:doc "Removes a prop image as well as any instances of that image
          in all scenes."}
  event-tx-fn :props-images/remove
  [data _ hash]
  (let [user (ds/entity data [:db/ident :user])
        xfrm (comp
              (mapcat (comp :scene/props :camera/scene))
              (filter (comp #{hash} :image/hash :prop/image)))]
    (into [[:db/retractEntity [:image/hash hash]]]
          (for [entity (sequence xfrm (:user/cameras user))]
            [:db/retractEntity (:db/id entity)]))))

;; --- Image Libraries ---
;; See ogres.app.library for the plain-EDN export/import file format and
;; its own sanitization -- this event only ever receives an already-
;; sanitized {:keys [gallery entries]} manifest, `gallery` one of
;; :token/:props/:scene.

(def ^:private library-gallery-attr
  {:token :root/token-images :props :root/props-images :scene :root/scene-images})

(defn ^:private anchor->vec2
  [[x y]]
  (Vec2. x y))

(defn ^:private library-entry-tx
  "One sanitized library entry (ogres.app.library/sanitize-entry) -> a
   seq of tx-data ops attaching/upgrading its image, or an empty seq to
   skip it outright. `existing` is (ds/entity data [:image/hash hash])
   or nil.

   Skipped when the hash isn't already a real local entity AND the entry
   carries no :location to fetch it from -- there's nothing to
   reconstruct pixels from, and a bare calibration-only entity would
   just be a permanently broken gallery thumbnail (provider/image.cljs's
   use-image falls back to requesting the image over the session relay,
   and there's no host to ever answer that request for data that was
   never really present on this install).

   An entry WITH a :location that isn't yet locally known gets a minimal
   entity -- hash, whatever metadata the exporter captured, and a
   thumbnail that self-references its own full image, the same graceful
   fallback use-image-url-adder itself falls back to when its own
   thumbnail-cropping request fails -- rather than re-running that crop/
   measure pipeline a second time here.

   :image/cell-px/:rotation/:anchor are filled in only when `existing`
   doesn't already have a value for them, unless `overwrite?` is true --
   :image/set-cell-scale and :image/set-rotation both retroactively
   rescale every already-placed instance sharing that hash (~1110-1154),
   so blindly overwriting on import would desync anything already on a
   live map."
  [existing overwrite?
   {:keys [hash location name width height size cell-px
           rotation anchor public default-label url]}]
  (if-not (or (some? existing) (some? location))
    []
    (let [fill? (fn [k v] (and (some? v) (or overwrite? (nil? (get existing k)))))]
      [(cond-> {:image/hash hash}
         (nil? existing)
         (assoc :image/thumbnail [:image/hash hash])
         (and (nil? existing) (some? name))
         (assoc :image/name name)
         (and (nil? existing) (some? width))
         (assoc :image/width width)
         (and (nil? existing) (some? height))
         (assoc :image/height height)
         (and (nil? existing) (some? size))
         (assoc :image/size size)
         (fill? :image/cell-px cell-px)
         (assoc :image/cell-px cell-px)
         (fill? :image/rotation rotation)
         (assoc :image/rotation rotation)
         (fill? :image/anchor anchor)
         (assoc :image/anchor (anchor->vec2 anchor))
         (fill? :image/public public)
         (assoc :image/public public)
         (fill? :token-image/default-label default-label)
         (assoc :token-image/default-label default-label)
         (fill? :token-image/url url)
         (assoc :token-image/url url))])))

(defmethod
  ^{:doc "Imports a previously-exported per-gallery image library (see
          ogres.app.library) into the given gallery. Host-gated, same
          convention as use-image-url-adder (image.cljs ~376-377) -- a
          library file is authored data, and there's no guest-to-host
          relay path for something this multi-step, unlike the simpler
          use-image-uploader relay.

          Attaches each resolvable entry's hash to this gallery's root
          collection -- idempotent, since :root/token-images and friends
          are all unique-identity component refs, so re-attaching an
          already-present hash merges rather than duplicates -- and see
          library-entry-tx for the per-entry fill-absent-only calibration
          and skip-when-unrecoverable behavior."}
  event-tx-fn :image-library/import
  [data _ {:keys [gallery entries]} overwrite?]
  (let [user (ds/entity data [:db/ident :user])
        root-attr (library-gallery-attr gallery)]
    (if (and (:user/host user) root-attr)
      (let [txs (into [] (mapcat (fn [entry]
                                    (library-entry-tx
                                     (ds/entity data [:image/hash (:hash entry)])
                                     overwrite? entry)))
                       entries)
            hashes (into [] (comp (map :image/hash) (distinct)) txs)]
        (if (seq hashes)
          (conj txs (assoc {:db/ident :root} root-attr (for [h hashes] {:image/hash h})))
          txs))
      [])))

;; --- Props ---

(defn ^:private image-cell-scale
  "The :object/scale a prop or board image should be placed at so that one
   grid cell's worth of the image covers one cell of the scene.

   Resolution order, most specific first:
     1. the image's own :image/cell-px, calibrated by scaling a placed
        copy and hitting 'Save scale as default' (:image/set-cell-scale);
     2. :root/default-cell-px -- the campaign-wide baseline, for the
        common case where a host's whole asset set was produced at one
        pixel density and calibrating each image individually is just
        busywork;
     3. 1, i.e. native pixels, which is what happened before either
        existed.

   Tokens deliberately do not participate: their footprint comes from
   :token/size in game units and the artwork is fitted to the token's
   circle, so source resolution never affects how big they render."
  [data cell-px]
  (let [px (or cell-px (:root/default-cell-px (ds/entity data [:db/ident :root])))]
    (if (and (number? px) (pos? px)) (/ grid-size px) 1)))

(defmethod
  ^{:doc "Creates a new prop image in the current scene at the given
          screen-space point. If the image has a saved cell-scale
          calibration (:image/cell-px, set via :image/set-cell-scale on
          some earlier placed copy), the prop drops pre-scaled to match
          the scene's grid instead of at native size (1:1). If the image
          has a saved default rotation (:image/rotation, set via
          :image/set-rotation), the prop also drops pre-rotated to match
          instead of always starting at 0 degrees. If grid-align is
          enabled, the prop's true center also snaps to the nearest grid
          cell, the same way tokens already do on drop."}
  event-tx-fn :props/create
  [data _ point hash]
  (let [{{bounds :user/bounds
          {camera-point :camera/point
           camera-scale :camera/scale
           camera-id :db/id
           {scene-id :db/id grid-type :scene/grid-type
            align? :scene/grid-align
            game-type-entity :scene/game-type}
           :camera/scene}
          :user/camera} :root/user}
        (ds/entity data [:db/ident :root])
        {width :image/width
         height :image/height
         cell-px :image/cell-px
         rotation :image/rotation
         anchor :image/anchor}
        (ds/entity data [:image/hash hash])
        scale (image-cell-scale data cell-px)
        rotation (or rotation 0)
        align? (and align? (pos? (game-type/grid-count (:game-type/enabled-elements game-type-entity #{}))))
        base-type (geom/base-grid-type grid-type)
        xform
        (-> (matrix/translate matrix/identity camera-point)
            (matrix/translate (/ width -2) (/ height -2))
            (matrix/multiply (geom/scene-scale-matrix camera-scale grid-type))
            (matrix/translate (vec/mul (.-a bounds) -1)))
        object-point (xform point)
        object-point
        (if align?
          (geom/snap-to-cell
           {:object/type :prop/prop
            :object/point object-point :object/scale scale :object/rotation rotation
            :prop/image {:image/width width :image/height height :image/anchor anchor}}
           vec/zero base-type)
          object-point)]
    (if (geom/point-within-rect? point bounds)
      [[:db/add -1 :object/point object-point]
       [:db/add -1 :object/scale scale]
       [:db/add -1 :object/rotation rotation]
       [:db/add -1 :object/type :prop/prop]
       [:db/add -1 :prop/image [:image/hash hash]]
       [:db/add camera-id :camera/selected -1]
       [:db/add scene-id :scene/props -1]]
      [])))

(defn ^:private image-calibration
  "The saved scale/rotation/anchor calibration for the image with the
   given :image/hash -- {:width :height :cell-px :rotation :anchor},
   all nil if that hash has never been referenced before. Deliberately
   checks existence via a raw index scan first rather than calling
   (ds/entity data [:image/hash hash]) directly and trusting a nil
   result: DataScript's entity throws for a lookup ref with no matching
   datoms at all, rather than returning nil, so calling it speculatively
   on a hash that might not exist yet (e.g. :props/create-many/
   :memory/start's freshly-generated, never-uploaded data: URI card
   faces) would crash instead of gracefully falling back to defaults."
  [data hash]
  (if (seq (ds/datoms data :avet :image/hash hash))
    (select-keys (ds/entity data [:image/hash hash])
                 [:image/width :image/height :image/cell-px :image/rotation :image/anchor])
    {}))

(defmethod
  ^{:doc "Creates `n` new, fully independent props in the current scene
          from a single image `hash`, in one transaction -- the
          scene-space counterpart of :clipboard/paste's per-copy minting
          loop, not a repeat of :props/create's one-at-a-time,
          screen-space/pointer-drop path (there's no pointer coordinate
          to convert here -- `point` is already scene-space).

          `opts`:
          - :layout    :stack (default -- every copy lands on the exact
                       same anchor-corrected point, a physical pile) or
                       :grid (row-major spread, :columns wide, :spacing
                       scene-units apart -- e.g. a Memory-style table
                       layout).
          - :columns/:spacing -- :grid layout geometry, default 8 /
                       grid-size.
          - :hidden?/:shared? -- baked into every copy's :object/hidden/
                       :object/shared? at mint time.
          - :alt-hash  -- every copy's :prop/image-alt, at mint time.
          - :tag-fn    -- (idx) -> map merged into every copy's
                       :object/variables on top of the automatic
                       {:prop/copy-index idx} tag -- the seam
                       :props/create-pile uses to inject :pile/id/
                       :pile/position without this event knowing
                       anything about piles.

          Same scale/rotation calibration and grid-align/snap-to-cell
          behavior as :props/create; :object/owner is never set (stays
          host-only by default, same as any newly created object)."}
  event-tx-fn :props/create-many
  ([data event point hash n]
   [[:db.fn/call event-tx-fn event point hash n {}]])
  ([data _ point hash n
    {:keys [layout columns spacing hidden? shared? alt-hash tag-fn]
     :or   {layout :stack columns 8 spacing grid-size
            hidden? false shared? false tag-fn (constantly nil)}}]
   (let [{{camera-id :db/id
           {scene-id :db/id grid-type :scene/grid-type align? :scene/grid-align
            game-type-entity :scene/game-type} :camera/scene}
          :user/camera} (ds/entity data [:db/ident :user])
         {width :image/width height :image/height cell-px :image/cell-px
          rotation :image/rotation anchor :image/anchor}
         (image-calibration data hash)
         scale (image-cell-scale data cell-px)
         rotation (or rotation 0)
         align? (and align? (pos? (game-type/grid-count (:game-type/enabled-elements game-type-entity #{}))))
         base-type (geom/base-grid-type grid-type)
         point-at
         (case layout
           :grid (fn [idx] (let [[dx dy] (props/grid-offset idx columns spacing)]
                              (vec/add point (Vec2. dx dy))))
           (constantly point))]
     (into
      []
      (mapcat
       (fn [[id idx]]
         (let [raw-point (point-at idx)
               object-point
               (if align?
                 (geom/snap-to-cell
                  {:object/type :prop/prop :object/point raw-point
                   :object/scale scale :object/rotation rotation
                   :prop/image {:image/width width :image/height height :image/anchor anchor}}
                  vec/zero base-type)
                 raw-point)]
           (cond-> [[:db/add id :object/type :prop/prop]
                    [:db/add id :object/point object-point]
                    [:db/add id :object/scale scale]
                    [:db/add id :object/rotation rotation]
                    [:db/add id :prop/image [:image/hash hash]]
                    [:db/add id :object/variables (merge {:prop/copy-index idx} (tag-fn idx))]
                    [:db/add scene-id :scene/props id]
                    [:db/add camera-id :camera/selected id]]
             hidden? (conj [:db/add id :object/hidden true])
             shared? (conj [:db/add id :object/shared? true])
             alt-hash (conj [:db/add id :prop/image-alt [:image/hash alt-hash]]))))
       (sequence (indexed) (range n)))))))

(defn ^:private scene-props
  "The :scene/props collection for the current camera's scene -- used by
   the pile events below to look up pile membership, mirroring how the
   abstract deck system's private `pile` helper filters :deck/cards."
  [data]
  (:scene/props (:camera/scene (:user/camera (ds/entity data [:db/ident :user])))))

(defmethod
  ^{:doc "Creates `n` new props from `hash`, all stamped onto one
          physical pile identified by `pile-id` (caller-supplied, e.g.
          (random-uuid) generated right before dispatch) -- a thin
          wrapper over :props/create-many with :layout :stack and a
          :tag-fn that assigns each copy a :pile/position 0..n-1 within
          the pile."}
  event-tx-fn :props/create-pile
  ([data event point hash n pile-id]
   [[:db.fn/call event-tx-fn event point hash n pile-id {}]])
  ([data _ point hash n pile-id opts]
   [[:db.fn/call event-tx-fn :props/create-many point hash n
     (assoc opts :tag-fn (fn [idx] {:pile/id pile-id :pile/position idx}))]]))

(defmethod
  ^{:doc "Moves the top-ordered prop (highest :pile/position, see
          ogres.app.props/top-of-pile) of the pile identified by
          `pile-id` to `target-point` (scene-space), or leaves it at its
          current point if `target-point` is nil, and un-piles it --
          clears just :pile/id/:pile/position (via
          :objects/merge-variables), preserving any other variables
          (e.g. :prop/copy-index). Deliberately does not touch
          :object/hidden -- 'drawing' and 'revealing' stay separate,
          composable actions; a caller issues :objects/toggle-hidden
          itself if it wants the drawn card to also flip face-up. A
          no-op if the pile has no members."}
  event-tx-fn :props/draw-from-pile
  [data _ pile-id target-point]
  (let [top (props/top-of-pile (props/pile (scene-props data) pile-id))]
    (if top
      (cond-> [[:db.fn/call event-tx-fn :objects/merge-variables [(:db/id top)]
                {:pile/id nil :pile/position nil}]]
        (some? target-point) (conj [:db/add (:db/id top) :object/point target-point]))
      [])))

(defmethod
  ^{:doc "Tags `prop-id` onto the pile identified by `pile-id` at
          next-pile-position (on top) and relocates it to the pile's
          existing anchor point (the first member's :object/point),
          re-stacking it visually -- the physical analogue of
          :deck/discard. If the pile has no existing members, `prop-id`
          simply becomes its first, at its own current point."}
  event-tx-fn :props/discard-to-pile
  [data _ prop-id pile-id]
  (let [members (props/pile (scene-props data) pile-id)]
    (cond-> [[:db.fn/call event-tx-fn :objects/merge-variables [prop-id]
              {:pile/id pile-id :pile/position (props/next-pile-position members)}]]
      (:object/point (first members))
      (conj [:db/add prop-id :object/point (:object/point (first members))]))))

;; --- Memory (example game) ---
;; A concrete, playable demonstration of the generic prop-copy/shared-
;; toggle mechanism above -- not part of the generic engine, the same
;; way D&D's d20 initiative roll and Gloomhaven's ability-deck rules are
;; game-specific layers on top of the generic turn-order/card systems.
;; See ogres.app.memory for the pure dealing/turn/scoring logic.
;;
;; The SECOND game ported onto the generic mini-game session scaffolding
;; (see :scene/minigames, above the Players section) -- and the first
;; with no deck at all. Its 'cards' are plain scene props (:scene/props
;; already owns them, :db/isComponent), so a session can't OWN them the
;; way Old Maid's session owns its deck; instead each session tracks
;; its own subset as BORROWED, non-component refs in :minigame/props,
;; mirroring how :scene/initiative already refs tokens it doesn't own.
;; :minigame/remove explicitly retracts them for exactly that reason
;; (see its own docstring). :memory/flip is dispatched from the canvas
;; (scene_context_menu.cljs), which has no notion of 'which session is
;; selected' -- it resolves the owning session itself, via the reverse
;; ref :minigame/_props, the same way :deck/discard already resolves a
;; card's owning deck via :deck/_cards; :memory/flip's own signature is
;; therefore UNCHANGED by this port.

(defn ^:private memory-cards
  "`minigame`'s own Memory cards -- :minigame/cards, a :db/isComponent
   collection the session owns outright. Deliberately NOT scene props:
   the cards are drawn as part of the session's own :minigame/table
   object, so nothing in the main game can select, drag, delete or sweep
   them, and :minigame/remove cascades to them for free."
  [minigame]
  (:minigame/cards minigame))

(defn ^:private memory-face-up
  "The subset of `minigame`'s memory-cards that are currently revealed
   -- 0, 1, or 2 at any time (:memory/flip refuses a 3rd until
   :memory/resolve clears the board back to 0 or 2->0). Face-up is the
   card's own :memory/face-up? flag rather than the generic
   :object/hidden, since these are no longer scene objects at all."
  [minigame]
  (filter :memory/face-up? (memory-cards minigame)))

(defn ^:private memory-player-active?
  "True if roster player-id refers to a still-existing, still-active
   (:player/active true) player -- used to let the Memory turn cycle
   skip over anyone benched or removed mid-game, see
   ogres.app.turn-order/valid-turn-index."
  [data player-id]
  (let [entity (ds/entity data player-id)]
    (boolean (and entity (:player/active entity)))))

(defn ^:private memory-turn-player
  "The roster player entity whose turn it currently is at `minigame`, or
   nil if every seated player has since been benched/removed. Resolves
   the CORRECTED index (skipping forward past anyone no longer active)
   rather than trusting the raw stored index directly, so a mid-game
   bench/remove/reactivate takes effect immediately without needing its
   own event."
  [data minigame]
  (let [players (mapv (comp :db/id :seat/player) (minigame-seats minigame))
        idx (:minigame/turn-index minigame)]
    (if (seq players)
      (let [corrected (turn-order/valid-turn-index players (partial memory-player-active? data) idx)]
        (if corrected
          (ds/entity data (nth players corrected)))))))

(defn ^:private memory-authorized-for-player?
  "True if the local viewer speaks for seated roster player
   `turn-player-id` at `minigame` -- the same player/authority? primitive
   :objects/toggle-hidden's authorized-to-hide? already uses for object
   ownership, just pointed at a turn seat instead of an object owner,
   fed that seat's effective controller (see minigame-controller-uuid).
   An unseated/nil player falls back to host-only, the same default
   authority? always uses.

   Takes the player explicitly rather than reading the current turn off
   `minigame`, because a flip that auto-resolves the previous turn has
   to be judged against whoever holds the turn AFTER that resolution --
   see :memory/flip."
  [data minigame turn-player-id]
  (let [user (ds/entity data [:db/ident :user])
        connected (into #{} (map :user/uuid) (:session/conns (ds/entity data [:db/ident :session])))
        seat (some #(if (= (:db/id (:seat/player %)) turn-player-id) %) (:minigame/seats minigame))]
    (player/authority? (:user/uuid user) (:user/host user) connected
                        (if seat (minigame-controller-uuid seat)))))

(defn ^:private memory-authorized-for-turn?
  "True if the local viewer speaks for `minigame`'s CURRENT turn player."
  [data minigame]
  (memory-authorized-for-player? data minigame (:db/id (memory-turn-player data minigame))))

(defn ^:private memory-settle
  "The transaction settling `minigame`'s outstanding turn, together with
   the id of the player whose turn it is once that settlement lands. nil
   when there is nothing to settle (fewer than two cards face-up).

   A match retracts both cards and scores the turn player, who plays
   again. A mismatch turns both back face-down and passes to the next
   active seat.

   Split out from :memory/resolve because settling is reachable two ways
   now: explicitly, via that event, and implicitly, when a player flips
   their next card while a finished turn is still lying on the table."
  [data minigame]
  (let [face-up (memory-face-up minigame)]
    (if (= (count face-up) 2)
      (let [[a b] face-up
            turn-id (:db/id (memory-turn-player data minigame))]
        (if (memory/pair? a b)
          (let [scores (or (:minigame/scores minigame) {})]
            ;; :minigame/cards is a component collection, so retracting
            ;; the card is the whole cleanup -- no scene-props bookkeeping.
            {:tx [[:db/retractEntity (:db/id a)]
                  [:db/retractEntity (:db/id b)]
                  {:db/id (:db/id minigame)
                   :minigame/scores (update scores turn-id (fnil inc 0))}]
             :turn-player-id turn-id})
          (let [players (mapv (comp :db/id :seat/player) (minigame-seats minigame))
                active? (partial memory-player-active? data)
                ;; Advance from the seat that actually just played, not
                ;; from the raw stored index -- the same reason
                ;; :old-maid/draw bases its step on `drawer-index`. When
                ;; the stored index points at a since-benched seat, the
                ;; turn player is resolved forward past them but the
                ;; stored index is not, so stepping from the raw value
                ;; hands the player who just missed a second turn.
                turn-idx (first (keep-indexed (fn [i id] (if (= id turn-id) i)) players))
                next-idx (or (turn-order/next-turn-index players active? turn-idx) turn-idx)]
            {:tx [[:db/retract (:db/id a) :memory/face-up?]
                  [:db/retract (:db/id b) :memory/face-up?]
                  {:db/id (:db/id minigame) :minigame/turn-index next-idx}]
             :turn-player-id (if (and next-idx (< next-idx (count players)))
                               (nth players next-idx))}))))))

(defmethod
  ^{:doc "Places an empty Memory table on the current scene: a
          :minigame/table scene object at the camera point, holding no
          cards and seating nobody. Rejected if :memory/game isn't
          enabled on the scene's own game-type (checked here, not just
          gated in the UI, since any connected participant may dispatch
          this).

          Placing and starting are two events on purpose. A table's
          dimensions lock as soon as cards are dealt onto it (see
          scale-locked?), so this is the state in which it can be
          dragged and resized to fit the scene -- position and size are
          settled first, then :memory/start deals into whatever frame
          was arranged.

          The felt is drawn at its full-deck footprint from the moment
          it is placed (memory/table-footprint), so what gets sized here
          is exactly what the deal will fill."}
  event-tx-fn :memory/place-table
  [data _ level]
  (let [user (ds/entity data [:db/ident :user])
        {point :camera/point scene :camera/scene} (:user/camera user)
        scene-id (:db/id scene)
        enabled (:game-type/enabled-elements (:scene/game-type scene))]
    (if (contains? enabled :memory/game)
      (let [minigame-id -1
            label (minigame-label (:scene/minigames scene) :memory "Memory")
            level (memory/clamp-difficulty level)
            [w h] (memory/table-footprint {:memory/difficulty level})
            scale memory/default-scale
            ;; A table scales about its middle (geom/object-transform),
            ;; so a shrunk one sits inset from its stored origin by half
            ;; the shrink. Offset the origin by that much and the table
            ;; lands where the camera is actually looking.
            inset (vec/Vec2. (* (/ w 2) (- 1 scale)) (* (/ h 2) (- 1 scale)))]
        (concat
         (minigame-create-tx minigame-id :memory label nil [])
         (minigame-attach-tx scene-id minigame-id)
         [{:db/id minigame-id
           :object/type :minigame/table
           :object/point (vec/sub point inset)
           :object/scale scale
           :memory/difficulty level}]))
      [])))

(defmethod
  ^{:doc "Resizes an as-yet-undealt table, from 32 cards (aces and court
          cards alone) up to 104 (every rank, both copies, all four
          suits) in steps of one numbered rank -- 8 cards a step, since
          a rank is 4 suits times 2 copies.

          Refused once cards are on the table: the size determines the
          felt's own footprint, so changing it mid-game would resize the
          board under the players, exactly what freezing a table in play
          exists to prevent (see memory/table-in-play?). End or clear
          the table to pick a different size."}
  event-tx-fn :memory/change-difficulty
  [data _ table-id level]
  (let [table (if (some? table-id) (ds/entity data table-id))]
    (if (and table
             (= (:object/type table) :minigame/table)
             (= (:minigame/kind table) :memory)
             (empty? (:minigame/cards table)))
      [[:db/add table-id :memory/difficulty (memory/clamp-difficulty level)]]
      [])))

(defmethod
  ^{:doc "Deals the already-placed table `table-id` at its own chosen
          size (see :memory/change-difficulty and ogres.app.memory/deal)
          -- two identical copies of every card in play, so a match is
          two cards that look the same -- seating exactly
          `participant-ids` -- rejected if none are given, if
          :memory/game isn't enabled on the scene's own game-type, if
          `table-id` isn't an empty Memory table, or if it already holds
          cards (dealing twice onto one table would strand the first
          deal's entities).

          Pairs match on rank AND colour, which splits one deck into
          exactly 26 pairs with nothing left over.

          The cards are not scene props and hold no coordinates: each
          carries a rank, a suit and a grid index, and the table's
          :object/point and :object/scale place them (memory/card-
          offset). That is what lets the whole board drag and scale as
          one unit, and what keeps every prop-wide action in the app
          away from a running game.

          Also switches the scene to :scene/neutral-authority? so the
          host gets no automatic X-ray over face-down cards -- an
          impartial-dealer table, see authorized-to-hide?."}
  event-tx-fn :memory/start
  [data _ table-id participant-ids]
  (let [user (ds/entity data [:db/ident :user])
        scene (:camera/scene (:user/camera user))
        enabled (:game-type/enabled-elements (:scene/game-type scene))
        table (if (some? table-id) (ds/entity data table-id))
        participant-ids (vec (distinct participant-ids))]
    (if (and (contains? enabled :memory/game)
             (>= (count participant-ids) 1)
             (= (:object/type table) :minigame/table)
             (= (:minigame/kind table) :memory)
             (empty? (:minigame/cards table)))
      (let [cards (memory/deal (memory/difficulty table))
            ;; Card ids -1..-52, then the seats past that whole block --
            ;; the same "reserve a block, then continue past it" idiom
            ;; deck-create-tx's card-ids-then-deck-id uses.
            card-ids (mapv (comp - inc) (range (count cards)))
            seat-base (- (inc (count cards)))
            seats (into []
                        (map-indexed
                         (fn [i player-id]
                           {:db/id (- seat-base i)
                            :seat/player player-id
                            :seat/order i}))
                        participant-ids)]
        (concat
         seats
         [{:db/id table-id
           :minigame/seats (mapv :db/id seats)
           :minigame/turn-index 0
           :minigame/cards card-ids}
          [:db.fn/call assoc-scene :scene/neutral-authority? true]]
         ;; No image entities at all: a card face is drawn inline from
         ;; its rank and suit, so a table pulls in nothing from the main
         ;; game's image gallery.
         (map (fn [id index card]
                (assoc card :db/id id :memory/index index))
              card-ids (range) cards)))
      [])))

(defmethod
  ^{:doc "Flips the given Memory card face-up -- a direct :object/hidden
          write, not a call through the generic :objects/toggle-hidden,
          since the turn-check (memory-authorized-for-turn?) IS this
          action's authorization (every Memory card is already :object/
          shared? true, so the generic authority check would pass for
          anyone regardless of turn -- this is what actually enforces
          'players take turns'). The owning session is resolved from
          `card-id` itself via the reverse ref :minigame/_props (the
          same way :deck/discard resolves a card's deck via :deck/
          _cards) -- dispatched from the canvas (scene_context_menu.
          cljs), which has no 'selected session' of its own to pass in,
          so this event's own signature stays exactly `card-id`, same
          as before session-scoping existed. A no-op if: `card-id`
          isn't a Memory card (or its session has since ended), the
          viewer doesn't speak for the current turn player, it's
          already face-up, or 2 cards are already face-up and awaiting
          :memory/resolve.

          Always retracts the local user's :user/dragging first, same
          as :objects/select -- this is dispatched from a zero-delta
          drag-kit gesture (use-drag-listener's onDragEnd 'click' case),
          which still fires a real onDragStart (:drag/start card-id)
          beforehand. Without this cleanup, :user/dragging would keep
          referencing card-id forever (nothing else ever retracts it
          for a click that isn't a genuine drag), and since :db/ident
          :user is peer-relative -- each peer's OWN conn entity is the
          only one tagged :db/ident :user locally -- that stale entry
          is invisible to the flipping viewer's own dragging filter but
          NOT to every other connected peer's (scene_objects.cljs's
          user-drag-xf), permanently locking this exact card out of
          drag-kit's draggable registration for everyone else."}
  event-tx-fn :memory/flip
  [data _ card-id]
  (let [card (ds/entity data card-id)
        ;; :minigame/cards is :db/isComponent, so this reverse ref
        ;; resolves to the single owning session directly.
        minigame (:minigame/_cards card)
        face-up (if minigame (memory-face-up minigame))
        ;; Turns settle themselves: a finished turn still lying on the
        ;; table is cleared by this very flip rather than by a separate
        ;; Resolve click, exactly as a player would sweep up the last
        ;; two cards as they reach for the next one.
        settle (if (= (count face-up) 2) (memory-settle data minigame))
        ;; Which means authority is judged against whoever holds the
        ;; turn AFTER that settlement -- on a mismatch that is the next
        ;; player, not the one who just missed. Checking the pre-settle
        ;; turn instead would let the missing player keep flipping and
        ;; lock everyone else out.
        turn-player-id (if settle
                         (:turn-player-id settle)
                         (:db/id (memory-turn-player data minigame)))]
    ;; The :user/dragging retract is unconditional and predates the
    ;; rebuild: a click on a card arrives through dnd-kit's own
    ;; zero-delta drag, so the drag has to be released whether or not
    ;; the flip itself is allowed.
    (into [[:db/retract [:db/ident :user] :user/dragging]]
          (if (and minigame
                   (memory-authorized-for-player? data minigame turn-player-id)
                   (some? (:card/rank card))
                   (not (:memory/face-up? card))
                   (<= (count face-up) 2))
            ;; Settling only happens alongside a flip that is actually
            ;; going through, so clicking an already face-up card (or
            ;; anything else refused above) leaves the table untouched.
            (concat (:tx settle) [[:db/add card-id :memory/face-up? true]])))))

(defmethod
  ^{:doc "Resolves `minigame-id`'s current turn once exactly 2 of its
          Memory cards are face-up (a no-op otherwise) -- same turn-
          authorization as :memory/flip. A match retracts both cards
          and increments the current turn player's tally in :minigame/
          scores, with the turn index UNCHANGED (matching players go
          again, same as a real game of Memory). A mismatch flips both
          back face-down and advances :minigame/turn-index to the next
          player, wrapping around (ogres.app.turn-order/next-turn-
          index).

          Turns normally settle themselves the moment somebody flips
          their next card (see :memory/flip and memory-settle), so this
          is the explicit path: useful to end a turn without starting
          the next one, and REQUIRED for the game's final pair, where
          there is no next card left to click and so nothing to trigger
          an automatic settlement."}
  event-tx-fn :memory/resolve
  [data _ minigame-id]
  (let [minigame (ds/entity data minigame-id)]
    (if (and minigame (memory-authorized-for-turn? data minigame))
      (or (:tx (memory-settle data minigame)) [])
      [])))

;; --- Go Fish (example game) ---
;; A second concrete demonstration of the generic card/deck system
;; (:card/holder-based hands, :deck/deal, move-card-tx) -- Memory never
;; needed private per-player hands or transferring a card between two
;; specific players; Go Fish exercises both, plus four independently
;; toggleable rules (see game_type/games/go_fish.cljs), checked live
;; from the active game-type's enabled-elements below rather than
;; stored anywhere new. See ogres.app.go-fish for the pure rank-
;; matching/scoring logic and ogres.app.turn-order for the shared
;; turn-cycle logic both this and Memory use.
;;
;; The THIRD game ported onto the generic mini-game session scaffolding
;; (see :scene/minigames, above the Players section) -- see :old-maid/
;; start's own docstring for the shape every ported game now shares.

(defn ^:private go-fish-player-active?
  "True if roster player-id refers to a still-existing, still-active
   (:player/active true) player -- same role as memory-player-active?,
   kept as its own function (not shared) since each game's turn-player
   resolution reads its own distinct seats, the same way dnd5e.cljs/
   gloomhaven.cljs each own their own similarly-shaped widget code
   rather than sharing one function."
  [data player-id]
  (let [entity (ds/entity data player-id)]
    (boolean (and entity (:player/active entity)))))

(defn ^:private go-fish-turn-player
  "The roster player entity whose turn it currently is at `minigame`,
   or nil if every seated player has since been benched/removed --
   resolves the corrected index the same way memory-turn-player does,
   see ogres.app.turn-order/valid-turn-index."
  [data minigame]
  (let [players (mapv (comp :db/id :seat/player) (minigame-seats minigame))
        idx (:minigame/turn-index minigame)]
    (if (seq players)
      (let [corrected (turn-order/valid-turn-index players (partial go-fish-player-active? data) idx)]
        (if corrected
          (ds/entity data (nth players corrected)))))))

(defn ^:private go-fish-authorized-for-turn?
  "True if the local viewer speaks for `minigame`'s current turn player
   -- same player/authority? primitive Memory's own memory-authorized-
   for-turn? uses, fed the current turn seat's effective controller
   (see minigame-controller-uuid), deliberately still just host-only
   fallback (not suppressed by :minigame/neutral-authority?) for the
   same turn-continuity-safety-net reason Memory's version is."
  [data minigame]
  (let [user (ds/entity data [:db/ident :user])
        connected (into #{} (map :user/uuid) (:session/conns (ds/entity data [:db/ident :session])))
        turn-player-id (:db/id (go-fish-turn-player data minigame))
        seat (some #(if (= (:db/id (:seat/player %)) turn-player-id) %) (:minigame/seats minigame))]
    (player/authority? (:user/uuid user) (:user/host user) connected
                        (if seat (minigame-controller-uuid seat)))))

(defn ^:private go-fish-hand
  "Every card in `player-id`'s hand, from a (ds/entity-pulled) `deck`."
  [deck player-id]
  (cards/cards-of-holder (:deck/cards deck) player-id))

(defmethod
  ^{:doc "Starts a new Go Fish session seated by exactly
          `participant-ids` -- rejected if fewer than 2 are given, or
          if :go-fish/game isn't actually enabled on the scene's own
          game-type (checked here, not just gated in the UI, since any
          connected participant may dispatch this). Creates a fresh
          36-card deck (9 ranks x 4 copies, see game-type.games.go-
          fish/deck-definitions), deals 6 cards to each participant's
          hand (:deck/deal, reused as-is), and starts the session's
          turn cycle at index 0. Anyone, not just the host, may start a
          table, same as :old-maid/start."}
  event-tx-fn :go-fish/start
  [data _ participant-ids]
  (let [user (ds/entity data [:db/ident :user])
        scene (:camera/scene (:user/camera user))
        enabled (:game-type/enabled-elements (:scene/game-type scene))
        participant-ids (vec (distinct participant-ids))]
    (if (and (contains? enabled :go-fish/game) (>= (count participant-ids) 2))
      (let [[deck-id deck-tx] (deck-create-tx :go-fish-9 {})
            cards (filter :card/rank deck-tx)
            label (minigame-label (:scene/minigames scene) :go-fish "Go Fish")
            minigame-id (dec deck-id)
            shell-tx (minigame-create-tx minigame-id :go-fish label deck-id participant-ids)]
        (concat
         deck-tx
         ;; Dealt directly from `cards` (plain data, not yet committed)
         ;; via deal-tx, NOT :deck/deal/:deck/draw -- deck-id is still
         ;; a temp id at this point in the transaction, which ds/entity
         ;; (what those events use to re-read the draw pile) can't
         ;; resolve. See deal-tx's docstring.
         (deal-tx cards participant-ids 6)
         shell-tx
         (minigame-attach-tx (:db/id scene) minigame-id)))
      [])))

(defmethod
  ^{:doc "The core Go Fish turn action within `minigame-id`: `asker-id`
          asks `target-id` for `rank`, which `asker-id` must already
          hold at least one copy of (a hard rule gate, not just a UI
          convenience -- enforced here the same way :memory/flip's own
          hidden/count checks are). `target-id` must be a different,
          active player, and -- unless :go-fish/ask-anyone is enabled
          -- specifically the next seat in the (skip-inactive) turn
          order. A hit transfers every matching card from target's
          hand to asker's; a miss draws the top of the draw pile into
          asker's hand instead ('go fish'), with no draw at all if the
          pile is empty. Whether the turn advances or stays with the
          asker depends on :go-fish/extra-turn-on-hit (a hit) and
          :go-fish/extra-turn-on-lucky-draw (drawing the exact rank
          asked for) -- a miss with no lucky draw always advances the
          turn, in every rule combination."}
  event-tx-fn :go-fish/ask
  [data _ minigame-id asker-id target-id rank]
  (let [minigame (ds/entity data minigame-id)]
    (if (and minigame
             (go-fish-authorized-for-turn? data minigame)
             (= (:db/id (go-fish-turn-player data minigame)) asker-id))
      (let [scene (:camera/scene (:user/camera (ds/entity data [:db/ident :user])))
            enabled (:game-type/enabled-elements (:scene/game-type scene))
            ask-anyone? (contains? enabled :go-fish/ask-anyone)
            extra-turn-on-hit? (contains? enabled :go-fish/extra-turn-on-hit)
            extra-turn-on-lucky-draw? (contains? enabled :go-fish/extra-turn-on-lucky-draw)
            players (mapv (comp :db/id :seat/player) (minigame-seats minigame))
            ;; The ASKER's own seat position, not :minigame/turn-index.
            ;; The guard above admits the player resolved through
            ;; go-fish-turn-player, which skips forward past benched
            ;; seats; the stored index does not move with it. Basing
            ;; `next-seat` and every advance below on the raw value meant
            ;; that with the stored index pointing at a benched seat, the
            ;; computed next seat could come out as the asker themselves
            ;; -- and since a clause below forbids targeting yourself,
            ;; NO target was legal and the table locked up permanently
            ;; (this event is the only writer of the index). Same
            ;; actor-relative basis :old-maid/draw and :crazy-eights/play
            ;; already use.
            idx (first (keep-indexed (fn [i id] (if (= id asker-id) i)) players))
            active? (partial go-fish-player-active? data)
            next-seat (turn-order/valid-turn-index players active? (mod (inc idx) (count players)))
            next-seat-id (if next-seat (nth players next-seat))
            deck-id (:db/id (:minigame/deck minigame))
            deck (ds/entity data deck-id)
            asker-hand (go-fish-hand deck asker-id)]
        (if (and (not= target-id asker-id)
                 (active? target-id)
                 (or ask-anyone? (= target-id next-seat-id))
                 (seq (cards/cards-of-rank asker-hand rank)))
          (let [target-hand (go-fish-hand deck target-id)
                matches (cards/cards-of-rank target-hand rank)]
            (if (seq matches)
              (let [start (next-position asker-hand)
                    transfer-tx (mapcat (fn [card i] (move-card-tx (:db/id card) :hand (+ start i) asker-id))
                                         matches (range))
                    next-idx (if extra-turn-on-hit? idx (turn-order/next-turn-index players active? idx))]
                (conj (vec transfer-tx) {:db/id minigame-id :minigame/turn-index (or next-idx idx)}))
              (let [draw-pile (pile deck :draw)]
                (if (seq draw-pile)
                  (let [drawn (top-card draw-pile)
                        lucky? (= (:card/rank drawn) rank)
                        moved (move-card-tx (:db/id drawn) :hand (next-position asker-hand) asker-id)
                        next-idx (if (and lucky? extra-turn-on-lucky-draw?)
                                   idx
                                   (turn-order/next-turn-index players active? idx))]
                    (conj (vec moved) {:db/id minigame-id :minigame/turn-index (or next-idx idx)}))
                  ;; empty draw pile -- no draw possible, a miss with
                  ;; nothing to fish for still just advances the turn.
                  (let [next-idx (turn-order/next-turn-index players active? idx)]
                    [{:db/id minigame-id :minigame/turn-index (or next-idx idx)}])))))
          []))
      [])))

(defmethod
  ^{:doc "Lays down a scored set from `player-id`'s own hand for `rank`
          within `minigame-id`, gated by player/authority? over that
          specific seat's effective controller (see minigame-
          controller-uuid) -- authority is derived from whose hand it
          is, not guessed from the dispatching viewer. NOT gated by
          whose turn it is -- laying down a completed set is
          bookkeeping, not a strategic action, and gating it by turn
          would just add 'forgot to score on my turn' friction with no
          fairness benefit. How many cards move, and whether a
          completed 4-card group is worth 1 point (a 'book', the
          classic rule) or 2 (two separate pairs) depends on :go-fish/
          book-scoring vs. :go-fish/pair-scoring -- see ogres.app.go-
          fish/scoreable-count. A no-op if `player-id` doesn't yet have
          an eligible set for `rank`, or isn't actually seated at this
          session."}
  event-tx-fn :go-fish/score
  [data _ minigame-id player-id rank]
  (let [minigame (ds/entity data minigame-id)
        user (ds/entity data [:db/ident :user])
        connected (into #{} (map :user/uuid) (:session/conns (ds/entity data [:db/ident :session])))
        seat (if minigame (some #(if (= (:db/id (:seat/player %)) player-id) %) (:minigame/seats minigame)))]
    (if (and seat (player/authority? (:user/uuid user) (:user/host user) connected (minigame-controller-uuid seat)))
      (let [enabled (:game-type/enabled-elements (:scene/game-type (:camera/scene (:user/camera user))))
            book-scoring? (contains? enabled :go-fish/book-scoring)
            deck-id (:db/id (:minigame/deck minigame))
            deck (ds/entity data deck-id)
            hand (go-fish-hand deck player-id)
            group (cards/cards-of-rank hand rank)
            n (go-fish/scoreable-count (count group) book-scoring?)]
        (if (pos? n)
          (let [to-score (take n group)
                scored (filter (comp #{:scored} :card/location) (:deck/cards deck))
                start (next-position scored)
                lay-tx (mapcat (fn [card i] (move-card-tx (:db/id card) :scored (+ start i) player-id))
                                to-score (range))
                scores (or (:minigame/scores minigame) {})
                ;; A completed book is always 1 point regardless of n
                ;; (n is always 4 in book mode) -- it's a single scored
                ;; unit, not "2 pairs bundled together". Pair mode's
                ;; point currency is the pair itself, so n/2 (n is
                ;; always even) counts every pair laid down at once.
                points (if book-scoring? 1 (quot n 2))]
            (conj (vec lay-tx) {:db/id minigame-id :minigame/scores (update scores player-id (fnil + 0) points)}))
          []))
      [])))

;; --- Old Maid (example game) ---
;; A third demonstration of the generic card/deck system -- dealing the
;; ENTIRE deck unevenly across players (deal-assignment with n nil), a
;; blind, targetless draw always from whoever's next in the turn cycle,
;; and turn-cycling that must skip ELIMINATED players (an empty hand),
;; not just benched ones -- expressed entirely by handing a richer
;; active? predicate to the existing, unmodified turn-order/valid-turn-
;; index/next-turn-index, exactly the extension seam those functions
;; were designed around; neither needed a single change. See
;; ogres.app.old-maid for the pure pair-detection logic.
;;
;; The FIRST game ported onto the generic mini-game session scaffolding
;; above (see :scene/minigames) -- a session-scoped prototype for
;; letting several independent, arbitrary-subset-of-the-roster tables
;; run nested inside one scene at once, rather than one singleton
;; instance assumed to include every active player. The other five
;; example games are unaffected and still use their own singleton
;; :scene/<game>-* attributes; they get ported the same way once this
;; shape proves out.

(defn ^:private old-maid-player-active?
  "True if roster player-id refers to a still-existing, still-active
   (:player/active true) player who ALSO still holds at least one
   card in `deck` -- an empty hand means that player is safe/
   eliminated for the rest of this round, and the turn cycle should
   skip them exactly like a benched player, with no separate
   'eliminate' event needed."
  [data deck player-id]
  (let [entity (ds/entity data player-id)]
    (and (boolean (and entity (:player/active entity)))
         (seq (cards/cards-of-holder (:deck/cards deck) player-id)))))

(defn ^:private old-maid-turn-player
  "The roster player entity whose turn it currently is at `minigame`,
   or nil if every seated player has since been benched/removed/
   eliminated -- resolves the corrected index the same way
   memory-turn-player/go-fish's equivalent do, see
   ogres.app.turn-order/valid-turn-index."
  [data minigame]
  (let [players (mapv (comp :db/id :seat/player) (minigame-seats minigame))
        idx (:minigame/turn-index minigame)
        deck (:minigame/deck minigame)]
    (if (seq players)
      (let [corrected (turn-order/valid-turn-index players (partial old-maid-player-active? data deck) idx)]
        (if corrected
          (ds/entity data (nth players corrected)))))))

(defn ^:private old-maid-authorized-for-turn?
  "True if the local viewer speaks for `minigame`'s current turn player
   -- same player/authority? primitive Memory/Go Fish's own authorized-
   for-turn? use, fed the current turn seat's effective controller (see
   minigame-controller-uuid, which layers a per-session override on top
   of the roster's own :player/controller), deliberately still just
   host-only fallback (not suppressed by :minigame/neutral-authority?)
   for the same turn-continuity-safety-net reason theirs are."
  [data minigame]
  (let [user (ds/entity data [:db/ident :user])
        connected (into #{} (map :user/uuid) (:session/conns (ds/entity data [:db/ident :session])))
        turn-player-id (:db/id (old-maid-turn-player data minigame))
        seat (some #(if (= (:db/id (:seat/player %)) turn-player-id) %) (:minigame/seats minigame))]
    (player/authority? (:user/uuid user) (:user/host user) connected
                        (if seat (minigame-controller-uuid seat)))))

(defmethod
  ^{:doc "Starts a new Old Maid session seated by exactly
          `participant-ids` (roster player ids, NOT necessarily every
          active player -- an arbitrary subset, the whole point of the
          mini-game session scaffolding above) -- rejected if fewer
          than 2 are given, or if :old-maid/game isn't actually enabled
          on the scene's own game-type (checked here, not just gated in
          the UI, since any connected participant may dispatch this).
          Creates a fresh 49-card deck (the traditional 52 minus 3
          queens, see game-type.games.old-maid/deck-definitions), deals
          the ENTIRE deck to the given participants (deal-assignment
          with n nil -- some getting one extra card when it doesn't
          divide evenly), and for each resulting hand immediately
          retracts any complete pairs it landed with (the mandatory
          'discard pairs before play begins' rule -- see ogres.app.
          old-maid/pairs-to-discard) instead of ever asserting them
          into a hand at all. The new session (see minigame-create-tx)
          starts at :minigame/turn-index 0 and :minigame/neutral-
          authority? true -- no per-game scores at all, since there's
          nothing numeric to track. Sets :scene/minigames and the
          creator's own :user/minigame-viewing to the new session --
          anyone, not just the host, may start a table."}
  event-tx-fn :old-maid/start
  [data _ participant-ids]
  (let [user (ds/entity data [:db/ident :user])
        scene (:camera/scene (:user/camera user))
        enabled (:game-type/enabled-elements (:scene/game-type scene))
        participant-ids (vec (distinct participant-ids))]
    (if (and (contains? enabled :old-maid/game) (>= (count participant-ids) 2))
      (let [[deck-id deck-tx] (deck-create-tx :old-maid-52 {})
            deck-map (first deck-tx)
            cards (filter :card/rank deck-tx)
            hands (deal-assignment cards participant-ids nil)
            ;; :db/retractEntity refuses a still-unresolved temp id
            ;; (unlike :db/add/map-form, which DOES resolve them) --
            ;; these cards are dealt-time pairs that should never
            ;; exist at all, so rather than assert-then-retract them
            ;; (which would fail), they're simply never asserted in
            ;; the first place: excluded from both their own per-card
            ;; tx-data AND the deck's own :deck/cards list below.
            discard-ids (into #{} (mapcat (fn [[_ hand]] (map :db/id (old-maid/pairs-to-discard hand)))) hands)
            card-tx (remove (comp discard-ids :db/id) cards)
            deck-map (update deck-map :deck/cards #(vec (remove discard-ids %)))
            keep-tx (mapcat (fn [[holder-id hand]]
                               (let [keep (remove (comp discard-ids :db/id) hand)]
                                 (map-indexed
                                  (fn [i card] {:db/id (:db/id card) :card/location :hand
                                                :card/holder holder-id :card/position i})
                                  keep)))
                             hands)
            label (minigame-label (:scene/minigames scene) :old-maid "Old Maid")
            minigame-id (dec deck-id)
            shell-tx (minigame-create-tx minigame-id :old-maid label deck-id participant-ids)]
        (concat
         [deck-map]
         card-tx
         keep-tx
         shell-tx
         (minigame-attach-tx (:db/id scene) minigame-id)))
      [])))

(defmethod
  ^{:doc "The core Old Maid turn action within `minigame-id`:
          `drawer-id` draws `card-id`, one specific (player-CHOSEN, not
          random) card from whoever is next in the (skip-eliminated)
          turn cycle after them -- 'the person to your right' and 'who
          plays next' are the same relationship once play moves in one
          consistent direction, so this reuses turn-order/valid-turn-
          index directly rather than inventing a separate 'neighbor'
          concept. The drawer picks WHICH of their neighbor's cards by
          position (component/card_hand.cljs's face-down, individually-
          clickable placeholders, wired up in panel_old_maid.cljs) --
          they still never see its rank/suit beforehand, so the outcome
          is exactly as blind as a truly random draw, but the choice of
          position itself is the player's, not the game's. `card-id`
          must actually belong to the resolved neighbor's hand -- a
          no-op otherwise (a stale/manipulated click, e.g. the UI's own
          rendered target going out of date). If the drawn card
          completes a pair in the drawer's hand, both cards are
          immediately retracted (ogres.app.old-maid/pairs-to-discard,
          reused from :old-maid/start) instead of the drawn card ever
          landing in the drawer's hand at all. The turn unconditionally
          advances to whoever was drawn from afterward -- no hit/miss
          branching, no extra-turn rule; Old Maid's turn logic is
          strictly simpler than Go Fish's by design, not by omission.
          A no-op if no other active player remains to draw from, or if
          `minigame-id` doesn't refer to a real session. The 'next'
          index is computed from the DRAWER's own resolved position
          among the session's seats -- never from the raw stored
          :minigame/turn-index -- because that stored index can go
          stale relative to the actual (skip-inactive) current player
          when a player is deactivated externally (e.g. benched from
          the roster) between draws; basing it on the drawer's own
          position keeps target != drawer even then."}
  event-tx-fn :old-maid/draw
  [data _ minigame-id drawer-id card-id]
  (let [minigame (ds/entity data minigame-id)]
    (if (and minigame
             (old-maid-authorized-for-turn? data minigame)
             (= (:db/id (old-maid-turn-player data minigame)) drawer-id))
      (let [players (mapv (comp :db/id :seat/player) (minigame-seats minigame))
            deck-id (:db/id (:minigame/deck minigame))
            deck (ds/entity data deck-id)
            active? (partial old-maid-player-active? data deck)
            drawer-index (first (keep-indexed (fn [i id] (if (= id drawer-id) i)) players))
            next-index (turn-order/valid-turn-index players active? (mod (inc drawer-index) (count players)))]
        (if next-index
          (let [target-id (nth players next-index)
                target-hand (cards/cards-of-holder (:deck/cards deck) target-id)
                drawn (some #(if (= (:db/id %) card-id) %) target-hand)]
            (if drawn
              (let [drawer-hand (cards/cards-of-holder (:deck/cards deck) drawer-id)
                    discard (old-maid/pairs-to-discard (conj (vec drawer-hand) drawn))]
                (concat
                 (if (seq discard)
                   (map (fn [c] [:db/retractEntity (:db/id c)]) discard)
                   (move-card-tx (:db/id drawn) :hand (next-position drawer-hand) drawer-id))
                 [{:db/id minigame-id :minigame/turn-index next-index}]))
              []))
          []))
      [])))

;; --- Crazy 8s (example game) ---
;; A fourth demonstration of the generic card/deck system -- the first
;; of the four to put cards face-up into a shared :card/location
;; :discard pile that every player matches against (Go Fish uses
;; :scored, Old Maid retracts pairs outright, Memory never discards),
;; and the first where the unit of action is a single specific card
;; (rank AND suit both matter) rather than a whole rank/hand. See
;; ogres.app.crazy-eights for the pure per-card legality logic.
;;
;; The FOURTH game ported onto the generic mini-game session
;; scaffolding (see :scene/minigames, above the Players section) -- see
;; :old-maid/start's own docstring for the shape every ported game now
;; shares.

(defn ^:private crazy-eights-player-active?
  "True if roster player-id refers to a still-existing, still-active
   (:player/active true) player -- plain roster-active, unlike Old
   Maid's richer version, because emptying your hand here WINS and
   ends the whole game rather than eliminating you into an ongoing
   round; there's no 'skip the winner, keep playing' concept to
   express."
  [data player-id]
  (let [entity (ds/entity data player-id)]
    (boolean (and entity (:player/active entity)))))

(defn ^:private crazy-eights-turn-player
  "The roster player entity whose turn it currently is at `minigame`,
   or nil if every seated player has since been benched/removed --
   resolves the corrected index the same way go-fish-turn-player/old-
   maid-turn-player do, see ogres.app.turn-order/valid-turn-index."
  [data minigame]
  (let [players (mapv (comp :db/id :seat/player) (minigame-seats minigame))
        idx (:minigame/turn-index minigame)]
    (if (seq players)
      (let [corrected (turn-order/valid-turn-index players (partial crazy-eights-player-active? data) idx)]
        (if corrected
          (ds/entity data (nth players corrected)))))))

(defn ^:private crazy-eights-authorized-for-turn?
  "True if the local viewer speaks for `minigame`'s current turn player
   -- same player/authority? primitive Go Fish/Old Maid's own
   authorized-for-turn? use, fed the current turn seat's effective
   controller (see minigame-controller-uuid), deliberately still just
   host-only fallback (not suppressed by :minigame/neutral-authority?)
   for the same turn-continuity-safety-net reason theirs are."
  [data minigame]
  (let [user (ds/entity data [:db/ident :user])
        connected (into #{} (map :user/uuid) (:session/conns (ds/entity data [:db/ident :session])))
        turn-player-id (:db/id (crazy-eights-turn-player data minigame))
        seat (some #(if (= (:db/id (:seat/player %)) turn-player-id) %) (:minigame/seats minigame))]
    (player/authority? (:user/uuid user) (:user/host user) connected
                        (if seat (minigame-controller-uuid seat)))))

(defmethod
  ^{:doc "Starts a new Crazy 8s session seated by exactly
          `participant-ids` -- rejected if fewer than 2 are given, or
          if :crazy-eights/game isn't actually enabled on the scene's
          own game-type (checked here, not just gated in the UI, since
          any connected participant may dispatch this). Creates a fresh
          52-card deck (the standard deck with suit-less 8s, see
          game-type.games.crazy-eights/deck-definitions), deals 6 cards
          to each participant (deal-assignment with n 6 -- untouched
          cards simply stay at :card/location :draw from deck-create-
          tx, no post-processing needed the way Old Maid's whole-deck
          deal requires), then flips the topmost remaining NON-8 card
          face up onto the discard pile as the starting card --
          skipping past any 8s so play never opens with a suit-less
          card and no declared suit yet (the digital equivalent of
          reshuffling a wild starter back in). The new session (see
          minigame-create-tx) starts at :minigame/turn-index 0,
          :minigame/suit (the starting card's own suit), and
          :minigame/neutral-authority? true -- no :minigame/winner at
          all until someone actually empties their hand. Anyone, not
          just the host, may start a table, same as :old-maid/start."}
  event-tx-fn :crazy-eights/start
  [data _ participant-ids]
  (let [user (ds/entity data [:db/ident :user])
        scene (:camera/scene (:user/camera user))
        enabled (:game-type/enabled-elements (:scene/game-type scene))
        participant-ids (vec (distinct participant-ids))]
    (if (and (contains? enabled :crazy-eights/game) (>= (count participant-ids) 2))
      (let [[deck-id deck-tx] (deck-create-tx :crazy-eights-52 {})
            deck-map (first deck-tx)
            cards (filter :card/rank deck-tx)
            hands (deal-assignment cards participant-ids 6)
            dealt-ids (into #{} (mapcat (fn [[_ hand]] (map :db/id hand))) hands)
            hand-tx (mapcat (fn [[holder-id hand]]
                               (map-indexed
                                (fn [i card] {:db/id (:db/id card) :card/location :hand
                                              :card/holder holder-id :card/position i})
                                hand))
                             hands)
            remaining (remove (comp dealt-ids :db/id) cards)
            starter (top-card (remove (comp #{:eight} :card/rank) remaining))
            discard-tx [{:db/id (:db/id starter) :card/location :discard :card/position 0}]
            label (minigame-label (:scene/minigames scene) :crazy-eights "Crazy 8s")
            minigame-id (dec deck-id)
            shell-tx (minigame-create-tx minigame-id :crazy-eights label deck-id participant-ids)]
        (concat
         [deck-map]
         cards
         hand-tx
         discard-tx
         shell-tx
         [{:db/id minigame-id :minigame/suit (:card/suit starter)}]
         (minigame-attach-tx (:db/id scene) minigame-id)))
      [])))

(defmethod
  ^{:doc "The core Crazy 8s turn action within `minigame-id`: `player-
          id` plays `card-id` from their own hand face up onto the
          discard pile, legal only per ogres.app.crazy-eights/playable?
          (checked here, server-side -- the panel's disabled state is
          convenience, not enforcement). `suit` names the suit to
          declare when `card-id` is an 8 (a wild, always legal);
          ignored otherwise, since a non-8's own :card/suit becomes the
          new thing to match against instead. If this empties the
          player's hand, they've won -- :minigame/winner is set and the
          turn index is left alone (the game is over, not paused);
          otherwise the turn unconditionally advances to the next
          active player. A no-op if it isn't `player-id`'s turn, or the
          card isn't theirs, or isn't currently legal to play. The
          'next' index is computed from the PLAYER's own resolved
          position among the session's seats -- never from the raw
          stored :minigame/turn-index -- for the same stale-index
          reason :old-maid/draw's fix applies: that stored index can go
          stale relative to the actual (skip-inactive) current player
          when someone is benched externally between turns."}
  event-tx-fn :crazy-eights/play
  [data _ minigame-id player-id card-id suit]
  (let [minigame (ds/entity data minigame-id)]
    (if (and minigame
             (nil? (:minigame/winner minigame))
             (crazy-eights-authorized-for-turn? data minigame)
             (= (:db/id (crazy-eights-turn-player data minigame)) player-id))
      (let [deck-id (:db/id (:minigame/deck minigame))
            deck (ds/entity data deck-id)
            hand (cards/cards-of-holder (:deck/cards deck) player-id)
            card (some #(if (= (:db/id %) card-id) %) hand)
            discard (pile deck :discard)
            top (top-card discard)
            declared-suit (:minigame/suit minigame)]
        (if (and card (crazy-eights/playable? card top declared-suit))
          (let [remaining-hand (remove (comp #{card-id} :db/id) hand)
                new-suit (if (= (:card/rank card) :eight) suit (:card/suit card))
                players (mapv (comp :db/id :seat/player) (minigame-seats minigame))
                active? (partial crazy-eights-player-active? data)
                player-index (first (keep-indexed (fn [i id] (if (= id player-id) i)) players))
                next-index (turn-order/next-turn-index players active? player-index)]
            (concat
             (move-card-tx card-id :discard (next-position discard) nil)
             [{:db/id minigame-id :minigame/suit new-suit}]
             (if (empty? remaining-hand)
               [{:db/id minigame-id :minigame/winner player-id}]
               (if next-index
                 [{:db/id minigame-id :minigame/turn-index next-index}]
                 []))))
          []))
      [])))

(defmethod
  ^{:doc "Draws exactly one card into `player-id`'s hand within
          `minigame-id` -- legal only when they currently hold NO
          playable card (see ogres.app.crazy-eights/playable-cards;
          drawing is never an optional escape hatch when a real play
          exists). Does NOT advance the turn -- one click, one card,
          same player's turn continues (they either draw again or, once
          able, play -- both separate dispatches), the literal 'draw
          cards until you're able to match or play an 8' rule rather
          than an auto-play. If the draw pile is empty, reshuffles the
          discard pile EXCEPT its live top card back into the draw pile
          first (effective-draw-pile, shared with Rummy's own draw-
          from-pile action). In the near-impossible case where even
          that leaves nothing to draw, the turn passes instead of
          deadlocking on a player who can neither play nor draw. A
          no-op once the game's already been won."}
  event-tx-fn :crazy-eights/draw
  [data _ minigame-id player-id]
  (let [minigame (ds/entity data minigame-id)]
    (if (and minigame
             (nil? (:minigame/winner minigame))
             (crazy-eights-authorized-for-turn? data minigame)
             (= (:db/id (crazy-eights-turn-player data minigame)) player-id))
      (let [deck-id (:db/id (:minigame/deck minigame))
            deck (ds/entity data deck-id)
            hand (cards/cards-of-holder (:deck/cards deck) player-id)
            discard (pile deck :discard)
            top (top-card discard)
            declared-suit (:minigame/suit minigame)]
        (if (seq (crazy-eights/playable-cards hand top declared-suit))
          []
          (let [{:keys [cards reshuffle-tx]} (effective-draw-pile deck)]
            (if (seq cards)
              (let [drawn (apply max-key :card/position cards)]
                (concat reshuffle-tx (move-card-tx (:db/id drawn) :hand (next-position hand) player-id)))
              (let [players (mapv (comp :db/id :seat/player) (minigame-seats minigame))
                    active? (partial crazy-eights-player-active? data)
                    player-index (first (keep-indexed (fn [i id] (if (= id player-id) i)) players))
                    next-index (turn-order/next-turn-index players active? player-index)]
                (if next-index
                  [{:db/id minigame-id :minigame/turn-index next-index}]
                  []))))))
      [])))

;; --- Rummy (example game) ---
;; A fifth demonstration of the generic card/deck system -- the first
;; with a SHARED, table-wide :card/location :scored area any player
;; can contribute to (not just whoever started it), and the first
;; where the game-ending condition (a hand empties) and the winner
;; (most scored cards) are genuinely different questions -- exactly
;; the shape turn-order/winners was built for and has had no real
;; consumer since Go Fish. See ogres.app.rummy for the pure set/run
;; detection logic.
;;
;; The FIFTH game ported onto the generic mini-game session scaffolding
;; (see :scene/minigames, above the Players section) -- see :old-maid/
;; start's own docstring for the shape every ported game now shares.

(defn ^:private rummy-player-active?
  "True if roster player-id refers to a still-existing, still-active
   (:player/active true) player -- plain roster-active, Crazy 8s'
   shape, not Old Maid's hand-emptiness-aware one: Rummy ends outright
   the instant a hand empties, there's no 'skip them, keep going'
   concept to express."
  [data player-id]
  (let [entity (ds/entity data player-id)]
    (boolean (and entity (:player/active entity)))))

(defn ^:private rummy-turn-player
  "The roster player entity whose turn it currently is at `minigame`,
   or nil if every seated player has since been benched/removed --
   resolves the corrected index the same way every prior game's
   turn-player fn does, see ogres.app.turn-order/valid-turn-index."
  [data minigame]
  (let [players (mapv (comp :db/id :seat/player) (minigame-seats minigame))
        idx (:minigame/turn-index minigame)]
    (if (seq players)
      (let [corrected (turn-order/valid-turn-index players (partial rummy-player-active? data) idx)]
        (if corrected
          (ds/entity data (nth players corrected)))))))

(defn ^:private rummy-authorized-for-turn?
  "True if the local viewer speaks for `minigame`'s current turn player
   -- same player/authority? primitive every prior game's authorized-
   for-turn? uses, fed the current turn seat's effective controller
   (see minigame-controller-uuid), deliberately still just host-only
   fallback (not suppressed by :minigame/neutral-authority?) for the
   same turn-continuity-safety-net reason theirs are."
  [data minigame]
  (let [user (ds/entity data [:db/ident :user])
        connected (into #{} (map :user/uuid) (:session/conns (ds/entity data [:db/ident :session])))
        turn-player-id (:db/id (rummy-turn-player data minigame))
        seat (some #(if (= (:db/id (:seat/player %)) turn-player-id) %) (:minigame/seats minigame))]
    (player/authority? (:user/uuid user) (:user/host user) connected
                        (if seat (minigame-controller-uuid seat)))))

(defn ^:private rummy-finished?
  "True once ANY of `players`' hands has emptied -- Rummy ends outright
   the instant this happens, guarding every :rummy/* action exactly
   the way Crazy 8s guards on its stored winner. Rummy has no stored
   winner at all (see :rummy/start's doc) -- who actually WON is a
   separate tally, computed live by panel_rummy.cljs from :card/holder
   on every :scored card, not decided here."
  [deck players]
  (boolean (some (fn [id] (empty? (cards/cards-of-holder (:deck/cards deck) id))) players)))

(defmethod
  ^{:doc "Starts a new Rummy session seated by exactly
          `participant-ids` -- rejected if fewer than 2 are given, or
          if :rummy/game isn't actually enabled on the scene's own
          game-type (checked here, not just gated in the UI, since any
          connected participant may dispatch this). Creates a fresh
          52-card deck (the standard deck, completely unmodified --
          the purest reuse case yet, see game-type.games.rummy), deals
          6 cards to each participant (deal-assignment with n 6, same
          as Go Fish/Crazy 8s), then flips the topmost remaining card
          face up onto the discard pile as the starting card -- any
          rank is fine here, unlike Crazy 8s' starter pick, since
          Rummy has no wild/suit-less card to skip past. The new
          session (see minigame-create-tx) starts at :minigame/turn-
          index 0, :minigame/drawn? false, and :minigame/neutral-
          authority? true -- no :minigame/winner/-scores at all:
          unlike Crazy 8s' single stored winner, Rummy's tally is
          derived live from :card/holder on every :scored card (see
          rummy-finished?/panel_rummy.cljs), since the game-ending
          player and the eventual winner are often different people
          here. Anyone, not just the host, may start a table, same as
          :old-maid/start."}
  event-tx-fn :rummy/start
  [data _ participant-ids]
  (let [user (ds/entity data [:db/ident :user])
        scene (:camera/scene (:user/camera user))
        enabled (:game-type/enabled-elements (:scene/game-type scene))
        participant-ids (vec (distinct participant-ids))]
    (if (and (contains? enabled :rummy/game) (>= (count participant-ids) 2))
      (let [[deck-id deck-tx] (deck-create-tx :standard-52 {})
            deck-map (first deck-tx)
            cards (filter :card/rank deck-tx)
            hands (deal-assignment cards participant-ids 6)
            dealt-ids (into #{} (mapcat (fn [[_ hand]] (map :db/id hand))) hands)
            hand-tx (mapcat (fn [[holder-id hand]]
                               (map-indexed
                                (fn [i card] {:db/id (:db/id card) :card/location :hand
                                              :card/holder holder-id :card/position i})
                                hand))
                             hands)
            remaining (remove (comp dealt-ids :db/id) cards)
            starter (top-card remaining)
            discard-tx [{:db/id (:db/id starter) :card/location :discard :card/position 0}]
            label (minigame-label (:scene/minigames scene) :rummy "Rummy")
            minigame-id (dec deck-id)
            shell-tx (minigame-create-tx minigame-id :rummy label deck-id participant-ids)]
        (concat
         [deck-map]
         cards
         hand-tx
         discard-tx
         shell-tx
         [{:db/id minigame-id :minigame/drawn? false}]
         (minigame-attach-tx (:db/id scene) minigame-id)))
      [])))

(defmethod
  ^{:doc "The first half of the core Rummy turn action within
          `minigame-id`: draws the top of the draw pile into `player-
          id`'s hand -- legal only once per turn (:minigame/drawn? must
          still be false) and only on their own turn. Reuses effective-
          draw-pile (shared with Crazy 8s' :crazy-eights/draw) for
          'reshuffle the discard pile minus its live top card when the
          draw pile runs dry'. Sets :minigame/drawn? true -- :rummy/
          discard checks this before allowing the mandatory end-of-turn
          discard, and clears it again once that fires. A no-op if the
          game's already finished, they've already drawn this turn, or
          there's truly nothing left to draw."}
  event-tx-fn :rummy/draw-from-pile
  [data _ minigame-id player-id]
  (let [minigame (ds/entity data minigame-id)
        deck-id (if minigame (:db/id (:minigame/deck minigame)))
        deck (if deck-id (ds/entity data deck-id))]
    (if (and minigame
             (not (rummy-finished? deck (mapv (comp :db/id :seat/player) (minigame-seats minigame))))
             (not (:minigame/drawn? minigame))
             (rummy-authorized-for-turn? data minigame)
             (= (:db/id (rummy-turn-player data minigame)) player-id))
      (let [hand (cards/cards-of-holder (:deck/cards deck) player-id)
            {:keys [cards reshuffle-tx]} (effective-draw-pile deck)]
        (if (seq cards)
          (let [drawn (apply max-key :card/position cards)]
            (concat reshuffle-tx
                    (move-card-tx (:db/id drawn) :hand (next-position hand) player-id)
                    [{:db/id minigame-id :minigame/drawn? true}]))
          []))
      [])))

(defmethod
  ^{:doc "The other half of the core turn action: takes the discard
          pile's current live top card into `player-id`'s hand instead
          of the draw pile -- same once-per-turn/turn-gating/finished
          guards as :rummy/draw-from-pile. A no-op if the discard pile
          is (momentarily) empty."}
  event-tx-fn :rummy/draw-from-discard
  [data _ minigame-id player-id]
  (let [minigame (ds/entity data minigame-id)
        deck-id (if minigame (:db/id (:minigame/deck minigame)))
        deck (if deck-id (ds/entity data deck-id))]
    (if (and minigame
             (not (rummy-finished? deck (mapv (comp :db/id :seat/player) (minigame-seats minigame))))
             (not (:minigame/drawn? minigame))
             (rummy-authorized-for-turn? data minigame)
             (= (:db/id (rummy-turn-player data minigame)) player-id))
      (let [hand (cards/cards-of-holder (:deck/cards deck) player-id)
            discard (pile deck :discard)]
        (if (seq discard)
          (let [top (top-card discard)]
            (concat (move-card-tx (:db/id top) :hand (next-position hand) player-id)
                    [{:db/id minigame-id :minigame/drawn? true}]))
          []))
      [])))

(defmethod
  ^{:doc "Lays down a scored set from `player-id`'s own hand for `rank`
          within `minigame-id`, gated by player/authority? over that
          specific seat's effective controller (see minigame-
          controller-uuid, same explicit player-id-arg pattern :go-
          fish/score uses) and NOT turn-gated -- laying down a
          completed set (or laying off the 4th onto an existing one)
          is bookkeeping any authorized player can do any time, the
          same call Go Fish already made for the identical reason. How
          many cards move depends on how many of `rank` are ALREADY
          scored on the table (ogres.app.rummy/scoreable-set): a fresh
          set takes everything the player holds (3 or 4 at once); once
          exactly 3 are already down, ANY player -- not just whoever
          scored the original 3 -- laying off the 4th needs just that
          one card. :card/holder still records who gets individual
          credit for each card laid down -- the win condition is 'most
          scored cards', not 'most complete sets', so who contributed
          which specific card is what matters, not who 'owns' a shared
          table group. A no-op if the game's finished, `player-id`
          isn't actually seated at this session, or they don't have an
          eligible set for `rank` right now."}
  event-tx-fn :rummy/score
  [data _ minigame-id player-id rank]
  (let [minigame (ds/entity data minigame-id)
        user (ds/entity data [:db/ident :user])
        connected (into #{} (map :user/uuid) (:session/conns (ds/entity data [:db/ident :session])))
        seat (if minigame (some #(if (= (:db/id (:seat/player %)) player-id) %) (:minigame/seats minigame)))]
    (if (and seat (player/authority? (:user/uuid user) (:user/host user) connected (minigame-controller-uuid seat)))
      (let [deck-id (:db/id (:minigame/deck minigame))
            deck (ds/entity data deck-id)]
        (if (rummy-finished? deck (mapv (comp :db/id :seat/player) (minigame-seats minigame)))
          []
          (let [hand (cards/cards-of-holder (:deck/cards deck) player-id)
                group (cards/cards-of-rank hand rank)
                scored (filter (comp #{:scored} :card/location) (:deck/cards deck))
                already-scored (rummy/set-scored-count (cards/cards-of-rank scored rank))
                n (rummy/scoreable-set (count group) already-scored)]
            (if (pos? n)
              (let [to-score (take n group)
                    start (next-position scored)]
                (mapcat (fn [card i] (move-card-tx (:db/id card) :scored (+ start i) player-id))
                        to-score (range)))
              []))))
      [])))

(defmethod
  ^{:doc "Lays down a run (3+ consecutive ranks, one suit) from
          `player-id`'s own hand within `minigame-id` -- only reachable
          when :rummy/runs is enabled (checked here server-side, the
          same live-from-enabled-elements pattern :go-fish/ask-anyone
          etc. use), and only for the EXACT `card-ids` given (validated
          directly against ogres.app.rummy/runs on the player's own
          hand right now, not re-derived from a suit/range -- the same
          'explicit ids, not re-derived' idiom :crazy-eights/play
          already uses). Same authority (not turn-gated) as :rummy/
          score. No lay-off-the-4th equivalent for runs -- out of
          scope, the user's own description of that rule was specific
          to sets. A no-op if the game's finished, the element's
          disabled, `player-id` isn't seated at this session, or
          `card-ids` doesn't exactly match one of the player's own
          current runs."}
  event-tx-fn :rummy/score-run
  [data _ minigame-id player-id card-ids]
  (let [minigame (ds/entity data minigame-id)
        user (ds/entity data [:db/ident :user])
        connected (into #{} (map :user/uuid) (:session/conns (ds/entity data [:db/ident :session])))
        seat (if minigame (some #(if (= (:db/id (:seat/player %)) player-id) %) (:minigame/seats minigame)))]
    (if (and seat (player/authority? (:user/uuid user) (:user/host user) connected (minigame-controller-uuid seat)))
      (let [scene (:camera/scene (:user/camera user))
            enabled (:game-type/enabled-elements (:scene/game-type scene))
            deck-id (:db/id (:minigame/deck minigame))
            deck (ds/entity data deck-id)]
        (if (and (contains? enabled :rummy/runs)
                 (not (rummy-finished? deck (mapv (comp :db/id :seat/player) (minigame-seats minigame)))))
          (let [hand (cards/cards-of-holder (:deck/cards deck) player-id)
                id-set (set card-ids)
                valid? (some #(= (set (map :db/id %)) id-set) (rummy/runs hand))]
            (if valid?
              (let [to-score (filter (comp id-set :db/id) hand)
                    scored (filter (comp #{:scored} :card/location) (:deck/cards deck))
                    start (next-position scored)]
                ;; Mark these as run-melds. The :scored area is shared and
                ;; otherwise makes a run's cards indistinguishable from a
                ;; laid-down set, which :rummy/score then miscounts as set
                ;; progress -- see rummy/set-scored-count.
                (mapcat (fn [card i]
                          (conj (vec (move-card-tx (:db/id card) :scored (+ start i) player-id))
                                [:db/add (:db/id card) :card/run-meld? true]))
                        to-score (range)))
              []))
          []))
      [])))

(defmethod
  ^{:doc "Ends `player-id`'s turn within `minigame-id`: discards
          `card-id` from their own hand face up onto the discard pile
          -- legal only once they've already drawn this turn
          (:minigame/drawn? true, the mandatory 'you must draw before
          you may discard' sequencing) and it's their turn. Clears
          :minigame/drawn? back to false and advances the turn
          unconditionally to the next active player -- resolved from
          the PLAYER's own resolved position among the session's seats,
          never the raw stored :minigame/turn-index, the same stale-
          index-safe pattern :old-maid/draw's fix and :crazy-eights/
          play both already apply. A no-op if the game's already
          finished, it isn't `player-id`'s turn, they haven't drawn
          yet, or `card-id` isn't actually in their hand."}
  event-tx-fn :rummy/discard
  [data _ minigame-id player-id card-id]
  (let [minigame (ds/entity data minigame-id)
        deck-id (if minigame (:db/id (:minigame/deck minigame)))
        deck (if deck-id (ds/entity data deck-id))]
    (if (and minigame
             (not (rummy-finished? deck (mapv (comp :db/id :seat/player) (minigame-seats minigame))))
             (:minigame/drawn? minigame)
             (rummy-authorized-for-turn? data minigame)
             (= (:db/id (rummy-turn-player data minigame)) player-id))
      (let [hand (cards/cards-of-holder (:deck/cards deck) player-id)
            card (some #(if (= (:db/id %) card-id) %) hand)]
        (if card
          (let [discard (pile deck :discard)
                players (mapv (comp :db/id :seat/player) (minigame-seats minigame))
                active? (partial rummy-player-active? data)
                player-index (first (keep-indexed (fn [i id] (if (= id player-id) i)) players))
                next-index (turn-order/next-turn-index players active? player-index)]
            (concat
             (move-card-tx card-id :discard (next-position discard) nil)
             [{:db/id minigame-id :minigame/drawn? false}]
             (if next-index
               [{:db/id minigame-id :minigame/turn-index next-index}]
               [])))
          []))
      [])))

;; --- War (example game) ---
;; A sixth demonstration of the generic card/deck system -- and the
;; first with NO shared pile at all (every card belongs to exactly one
;; player's own :draw/:won piles from deal to game-end) and NO turn
;; order (every active player plays simultaneously, every round --
;; ogres.app.turn-order is genuinely unused here, unlike every prior
;; game). See ogres.app.war for the pure tie-detection logic.
;;
;; The SIXTH and last game ported onto the generic mini-game session
;; scaffolding (see :scene/minigames, above the Players section) -- see
;; :old-maid/start's own docstring for the shape every ported game now
;; shares. War's own :scene/war-contenders/-last-round become
;; :minigame/contenders/-last-round, plain scalars on the session
;; entity exactly like every other game's own extra state.

(defn ^:private player-cards
  "`player-id`'s cards in `deck` currently at `location` (:draw or
   :won -- War's two personal, per-player piles)."
  [deck location player-id]
  (filter (fn [c] (and (= (:card/location c) location)
                       (= (:db/id (:card/holder c)) player-id)))
          (:deck/cards deck)))

(defn ^:private war-player-active?
  "True if roster player-id refers to a still-existing, still-active
   (:player/active true) player who ALSO still holds at least one
   card, in EITHER personal pile -- Old Maid's exact 'richer active?'
   shape (elimination-by-emptiness), just checking two locations
   instead of one, since War has no single 'hand' to check."
  [data deck player-id]
  (let [entity (ds/entity data player-id)]
    (and (boolean (and entity (:player/active entity)))
         (or (seq (player-cards deck :draw player-id))
             (seq (player-cards deck :won player-id))))))

(defn ^:private initial-piles
  "The current {player-id {:draw [...] :won [...]}} state for each of
   `player-ids`, read once from `deck` (:draw ordered top-first) --
   plain in-memory data (not tx), the same 'reason about the result
   before finalizing tx-data' idiom deal-assignment/effective-draw-
   pile already use, since a single :war/play-round call may need to
   draw and reshuffle several times in a row before it can commit
   anything."
  [deck player-ids]
  (into {}
        (map (fn [pid]
               [pid {:draw (vec (sort-by :card/position > (player-cards deck :draw pid)))
                     :won (vec (player-cards deck :won pid))}]))
        player-ids))

(defn ^:private draw-one
  "Pops the top card off `player-id`'s entry in `piles` (see initial-
   piles), reshuffling their :won pile into a freshly shuffled :draw
   first if :draw is empty -- the personal-pile analog of effective-
   draw-pile, deliberately kept separate from it: that one reshuffles
   a SHARED, holder-less :discard into a shared :draw; this reshuffles
   one specific player's OWN :won into their OWN :draw, a genuinely
   different filter, not worth forcing into one shared function for a
   third time. Returns [drawn-card-or-nil piles'] -- drawn-card is nil
   only if the player has truly nothing left in EITHER pile, in which
   case piles' is unchanged and they're simply excluded from this
   round (eliminated)."
  [piles player-id]
  (let [{:keys [draw won]} (get piles player-id)]
    (cond
      (seq draw) [(first draw) (assoc-in piles [player-id :draw] (vec (rest draw)))]
      (seq won) (let [reshuffled (shuffle won)]
                  [(first reshuffled) (assoc piles player-id {:draw (vec (rest reshuffled)) :won []})])
      :else [nil piles])))

(defn ^:private piles-tx
  "Tx-data committing every card still sitting in `piles` (see
   initial-piles/draw-one) back to its owner's :draw/:won piles --
   NOT the cards that were drawn out of them this round (those are
   handled separately, see :war/play-round, since where they end up
   depends on how the round resolves)."
  [piles]
  (mapcat (fn [[player-id {:keys [draw won]}]]
             (concat
              (map-indexed (fn [i card] {:db/id (:db/id card) :card/location :draw
                                          :card/holder player-id :card/position i})
                           (reverse draw))
              (map-indexed (fn [i card] {:db/id (:db/id card) :card/location :won
                                          :card/holder player-id :card/position i})
                           won)))
          piles))

(defmethod
  ^{:doc "Starts a new War session seated by exactly `participant-ids`
          -- rejected if fewer than 2 are given, or if :war/game isn't
          actually enabled on the scene's own game-type (checked here,
          not just gated in the UI, since any connected participant may
          dispatch this). Creates a fresh 52-card deck (the standard
          deck, completely unmodified -- same reuse tier as Rummy), and
          deals the ENTIRE deck to every participant (deal-assignment
          with n nil, Old Maid's exact call) straight into :card/
          location :draw -- no starting flip, since War has no shared
          discard pile at all. The new session (see minigame-create-tx)
          leaves :minigame/contenders/-last-round unset (DataScript
          rejects storing a literal nil via map-form assertion;
          :minigame/remove retracts the whole session anyway, so
          there's nothing to separately clear). Anyone, not just the
          host, may start a table, same as :old-maid/start."}
  event-tx-fn :war/start
  [data _ participant-ids]
  (let [user (ds/entity data [:db/ident :user])
        scene (:camera/scene (:user/camera user))
        enabled (:game-type/enabled-elements (:scene/game-type scene))
        participant-ids (vec (distinct participant-ids))]
    (if (and (contains? enabled :war/game) (>= (count participant-ids) 2))
      (let [[deck-id deck-tx] (deck-create-tx :standard-52 {})
            deck-map (first deck-tx)
            cards (filter :card/rank deck-tx)
            hands (deal-assignment cards participant-ids nil)
            hand-tx (mapcat (fn [[holder-id hand]]
                               (map-indexed
                                (fn [i card] {:db/id (:db/id card) :card/location :draw
                                              :card/holder holder-id :card/position i})
                                hand))
                             hands)
            label (minigame-label (:scene/minigames scene) :war "War")
            minigame-id (dec deck-id)
            shell-tx (minigame-create-tx minigame-id :war label deck-id participant-ids)]
        (concat
         [deck-map]
         cards
         hand-tx
         shell-tx
         (minigame-attach-tx (:db/id scene) minigame-id)))
      [])))

(defmethod
  ^{:doc "The only human action in the whole game: advances `minigame-
          id`'s War table by one comparison step. No per-seat
          authorization at all -- there's nothing to authorize (nobody
          chooses a card), the same trust-the-UI-to-gate-it precedent
          :*/start/:minigame/remove already set. Contenders are
          :minigame/contenders if a war is already in progress (a prior
          tie), else every currently-active seat -- this is what makes
          a tie resolve ONE ESCALATION PER CALL rather than looping the
          whole chain internally: a second :war/play-round dispatch is
          required to continue it, matching a real game of War actually
          feeling like several rounds rather than one silent jump to
          the result.

          Each contender draws their own top :draw card (reshuffling
          their own :won pile into a fresh :draw first if empty, see
          draw-one) -- a contender with NOTHING left in either pile is
          simply dropped from contention (eliminated), no separate
          event needed. Every card drawn moves face up to :card/
          location :war (the shared pot), still holder-tagged so the
          UI can show who played what. Then:
            - a unique highest card sweeps the ENTIRE :war pile (this
              click's cards plus everything already sitting there from
              earlier escalations) to that player's :won pile,
              :minigame/contenders clears, and :minigame/last-round
              records the winner and how many cards they just won;
            - 2+ tied for highest sets :minigame/contenders to exactly
              that tied set, leaving the pot in place for the next call
              to continue;
            - nobody could draw at all (every remaining contender
              simultaneously out of cards -- fully degenerate) is a
              no-op, left for a human to notice and end the table."}
  event-tx-fn :war/play-round
  [data _ minigame-id]
  (let [minigame (ds/entity data minigame-id)]
    (if (nil? minigame)
      []
      (let [players (mapv (comp :db/id :seat/player) (minigame-seats minigame))
            deck-id (:db/id (:minigame/deck minigame))
            deck (ds/entity data deck-id)
            active? (partial war-player-active? data deck)
            contenders (let [c (:minigame/contenders minigame)] (if (seq c) c (filter active? players)))]
        (if (<= (count contenders) 1)
          []
          (let [piles0 (initial-piles deck contenders)
                {:keys [piles drawn]}
                (reduce (fn [{:keys [piles drawn]} pid]
                          (let [[card piles'] (draw-one piles pid)]
                            {:piles piles' :drawn (if card (assoc drawn pid card) drawn)}))
                        {:piles piles0 :drawn {}}
                        contenders)
                remaining-pile-tx (piles-tx piles)]
            (if (empty? drawn)
              []
              (let [war-pile (pile deck :war)
                    war-start (next-position war-pile)
                    move-to-war-tx (map-indexed
                                     (fn [i [pid card]] {:db/id (:db/id card) :card/location :war
                                                          :card/holder pid :card/position (+ war-start i)})
                                     drawn)
                    tied (war/tied-for-highest drawn)]
                (if (= (count tied) 1)
                  (let [winner-id (first tied)
                        whole-pot (concat war-pile (vals drawn))
                        won-start (count (:won (get piles winner-id)))
                        award-tx (map-indexed
                                  (fn [i card] {:db/id (:db/id card) :card/location :won
                                                :card/holder winner-id :card/position (+ won-start i)})
                                  whole-pot)]
                    (concat remaining-pile-tx move-to-war-tx award-tx
                            [[:db/retract minigame-id :minigame/contenders]
                             {:db/id minigame-id
                              :minigame/last-round {:winner-id winner-id :cards-won (count whole-pot)}}]))
                  (concat remaining-pile-tx move-to-war-tx
                          [{:db/id minigame-id :minigame/contenders (vec tied)}]))))))))))

;; --- Board ---

(defmethod
  ^{:doc "Creates a new board (map/background) piece in the current scene
          at the given screen-space point. Same calibrated-scale and
          grid-align-snap behavior as :props/create -- see its doc."}
  event-tx-fn :board/create
  [data _ point hash]
  (let [{{bounds :user/bounds
          {camera-point :camera/point
           camera-scale :camera/scale
           camera-id :db/id
           {scene-id :db/id grid-type :scene/grid-type
            align? :scene/grid-align
            game-type-entity :scene/game-type}
           :camera/scene}
          :user/camera} :root/user}
        (ds/entity data [:db/ident :root])
        {width :image/width
         height :image/height
         cell-px :image/cell-px
         anchor :image/anchor}
        (ds/entity data [:image/hash hash])
        scale (image-cell-scale data cell-px)
        align? (and align? (pos? (game-type/grid-count (:game-type/enabled-elements game-type-entity #{}))))
        base-type (geom/base-grid-type grid-type)
        rotation-mode (if (= base-type :square) 90 60)
        xform
        (-> (matrix/translate matrix/identity camera-point)
            (matrix/translate (/ width -2) (/ height -2))
            (matrix/multiply (geom/scene-scale-matrix camera-scale grid-type))
            (matrix/translate (vec/mul (.-a bounds) -1)))
        object-point (xform point)
        object-point
        (if align?
          (geom/snap-to-cell
           {:object/type :board/piece
            :object/point object-point :object/scale scale :object/rotation 0
            :board/image {:image/width width :image/height height :image/anchor anchor}}
           vec/zero base-type)
          object-point)]
    (if (geom/point-within-rect? point bounds)
      [[:db/add -1 :object/point object-point]
       [:db/add -1 :object/scale scale]
       [:db/add -1 :object/rotation-mode rotation-mode]
       [:db/add -1 :object/type :board/piece]
       [:db/add -1 :board/image [:image/hash hash]]
       [:db/add camera-id :camera/selected -1]
       [:db/add scene-id :scene/board -1]]
      [])))
