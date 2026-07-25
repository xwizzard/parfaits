(ns ogres.app.events
  (:require [datascript.core :as ds]
            [clojure.set :refer [union difference]]
            [clojure.string :refer [trim]]
            [ogres.app.cards :as cards]
            [ogres.app.const :refer [grid-size hex-radius]]
            [ogres.app.game-type :as game-type]
            [ogres.app.geom :as geom]
            [ogres.app.matrix :as matrix]
            [ogres.app.player :as player]
            [ogres.app.props :as props]
            [ogres.app.segment :as seg]
            [ogres.app.vec :as vec :refer [Vec2]]))

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
          then switches them to it."}
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
   [:db/add -4 :camera/scene -2]
   [:db/add -4 :camera/point vec/zero]])

(defmethod
  ^{:doc "Switches to the given scene by the given camera identifier."}
  event-tx-fn :scenes/change
  [_ _ id]
  [{:db/ident :user :user/camera id}])

(defmethod
  ^{:doc "Removes the scene and corresponding camera for the local user. Also
          removes all scene cameras for any connected users and switches them
          to whichever scene the host is now on."}
  event-tx-fn :scenes/remove
  [data _ camera-id]
  (let [root (ds/entity data [:db/ident :root])
        user (ds/entity data [:db/ident :user])
        prev-cam (ds/entity data camera-id)
        prev-scn (:db/id (:camera/scene prev-cam))]
    (conj
     (if (= (:db/id (:user/camera user)) (:db/id prev-cam))
       (if-let [next-scn (:db/id (first (remove (comp #{prev-scn} :db/id) (:root/scenes root))))]
         (if-let [next-cam (:db/id (first (filter (comp #{next-scn} :db/id :camera/scene) (:user/cameras user))))]
           [[:db/add (:db/id user) :user/camera next-cam]]
           [[:db/add (:db/id user) :user/camera -1]
            [:db/add (:db/id user) :user/cameras -1]
            [:db/add -1 :camera/scene next-scn]
            [:db/add -1 :camera/point vec/zero]])
         [[:db/add (:db/id root) :root/scenes -2]
          [:db/add (:db/id user) :user/camera -1]
          [:db/add (:db/id user) :user/cameras -1]
          [:db/add -1 :camera/scene -2]
          [:db/add -1 :camera/point vec/zero]
          [:db/add -2 :db/empty true]]) [])
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
  ^{:doc "Updates the grid size for the current scene."}
  event-tx-fn :scene/change-grid-size
  [_ _ size]
  [[:db.fn/call assoc-scene :scene/grid-size size]])

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
                :let [{id :db/id point :object/point type :object/type} entity]]
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
   false, behavior is exactly as before this flag existed."
  [data entity]
  (or (:object/shared? entity)
      (let [user (ds/entity data [:db/ident :user])
            connected (into #{} (map :user/uuid) (:session/conns (ds/entity data [:db/ident :session])))
            controller-uuid (get-in entity [:object/owner :player/controller :user/uuid])]
        (player/authority? (:user/uuid user) (:user/host user) connected controller-uuid))))

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
  [_ _ idxs]
  (for [id idxs]
    [:db/retractEntity id]))

(defmethod
  ^{:doc "Removes all currently currently selected objects."}
  event-tx-fn :objects/remove-selected
  [data _]
  (let [user (ds/entity data [:db/ident :user])]
    (for [{id :db/id} (:camera/selected (:user/camera user))]
      [:db/retractEntity id])))

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
  (let [parsed (.parseFloat js/window rank)]
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
          one undo entry, instead of N separate :initiative/change-rank
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
  ([_ _ deck-key {:keys [include-extras? label]}]
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
     (concat [{:db/id deck-id
               :deck/name (or label (:deck/name definition))
               :deck/cards ids}]
             card-tx
             [[:db.fn/call assoc-scene :scene/decks deck-id]]))))

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
        deck (first (:deck/_cards card))]
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
  [_ _ player-id user-id]
  (if user-id
    [{:db/id player-id :player/controller user-id}]
    [[:db/retract player-id :player/controller]]))

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
  [_ _ hash thumb rect]
  [{:image/hash hash
    :image/thumbnail-rect rect
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

;; --- Props ---

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
        scale (if cell-px (/ grid-size cell-px) 1)
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
         (ds/entity data [:image/hash hash])
         scale (if cell-px (/ grid-size cell-px) 1)
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
        scale (if cell-px (/ grid-size cell-px) 1)
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
