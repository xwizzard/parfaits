(ns ogres.app.component.scene
  (:require [clojure.set :refer [difference]]
            [clojure.string :refer [join]]
            [goog.object :refer [getValueByKeys]]
            [ogres.app.component :refer [icon image]]
            [ogres.app.component.scene-draw :refer [draw]]
            [ogres.app.component.scene-objects :refer [objects]]
            [ogres.app.component.scene-pattern :refer [pattern]]
            [ogres.app.const :refer [grid-size half-size hex-width hex-row token-radius]]
            [ogres.app.game-type :as game-type]
            [ogres.app.geom :as geom]
            [ogres.app.hooks :as hooks]
            [ogres.app.modifiers :as modifiers]
            [ogres.app.segment :as seg :refer [Segment]]
            [ogres.app.svg :refer [circle->path poly->path]]
            [ogres.app.util :refer [key-by]]
            [ogres.app.vec :as vec :refer [Vec2]]
            [react-transition-group :refer [TransitionGroup Transition CSSTransition]]
            [uix.core :as uix :refer [defui $]]
            ["@rwh/react-keystrokes" :refer [useKey] :rename {useKey use-key}]
            ["@dnd-kit/core"
             :refer [DndContext useDraggable]
             :rename {DndContext dnd-context useDraggable use-draggable}]))

(defn ^:private token-light-xf [host]
  (comp (filter
         (fn [token]
           (and (> (:token/light token) 0)
                (or host (not (:object/hidden token))))))
        (map
         (fn [token]
           [(.-x (:object/point token))
            (.-y (:object/point token))
            (+ (/ (* (:token/light token) grid-size) 5) grid-size)]))))

(def ^:private mask-area-xf
  (comp (filter :mask/enabled?) (map :mask/vecs)))

(def ^:private draw-modes
  #{:grid :ruler :circle :rect :cone :line :poly :mask :note})

;; The turn-order indicator is a core flag (driven by :scene/_initiative,
;; see token-flags below), not a game-specific status -- it always has an
;; icon available regardless of which :token-badge elements a game-type
;; enables. Everything else comes from whichever enabled elements declare
;; a :token-badge vocabulary (see badge-icons below); this file never
;; names a specific game or condition/status-effect vocabulary directly.
(def ^:private core-badge-icons
  {:initiative {:icon "hourglass-split"}})

(defn ^:private badge-icons
  "A flat {value {:icon :color :badge-color :shape}} map assembled from
   every enabled element's :token-badge vocabulary, plus the always-
   available core turn-order icon. `:color` is nil (inherit, same as
   today) unless the vocabulary entry names one. `:badge-color` fills the
   badge's backdrop rather than the glyph, and `:shape` selects that
   backdrop's form -- it is declared once on the :token-badge map, not per
   entry, and is folded onto each entry here so the render site below can
   stay a flat lookup. Different game-types can (and do) supply entirely
   different vocabularies here -- see ogres.app.game-type.games.dnd5e/
   gloomhaven, the latter drawing its statuses on diamonds."
  [enabled-elements]
  (into core-badge-icons
        (mapcat
         (fn [[_ element]]
           (let [{:keys [vocabulary shape]} (:token-badge element)]
             (map (fn [entry]
                    [(:value entry)
                     (cond-> (select-keys entry [:icon :color :badge-color])
                       (some? shape) (assoc :shape shape))])
                  vocabulary))))
        (filter (comp :token-badge val) (select-keys game-type/elements enabled-elements))))

(defn ^:private stop-propagation [event]
  (.stopPropagation event))

(defn ^:private token-label
  [{:keys [token/label initiative/suffix]}]
  (cond-> ""
    (string? label) (str label)
    (number? suffix) (str " " (char (+ suffix 64)))))

(def ^:private board-bounds-query
  [{:user/camera
    [{:camera/scene
      [{:scene/board
        [[:object/type :default :board/piece]
         [:object/point :default vec/zero]
         [:object/scale :default 1]
         [:object/rotation :default 0]
         {:board/image [[:image/width :default 0] [:image/height :default 0]]}]}]}]}])

(defui ^:private image-defs
  "Defines the scene's masking/lighting bounds (#scene-image-cover,
   #scene-image-clip) as the union of every placed board piece's own
   (rotated/scaled) bounding rect -- there's no longer one single
   full-scene image to size the scene off of, so the playable/maskable
   area grows and shrinks automatically as board pieces are placed,
   moved, or removed. With no pieces placed at all, falls back to one
   grid cell rather than a degenerate zero-size rect."
  []
  (let [result (hooks/use-query board-bounds-query)
        board (-> result :user/camera :camera/scene :scene/board)
        bound (if (seq board)
                (transduce (mapcat geom/object-bounding-rect) geom/bounding-rect-rf board)
                (Segment. vec/zero (Vec2. grid-size grid-size)))]
    ($ :defs
      ($ :filter {:id "scene-image-filter" :filterRes 1 :color-interpolation-filters "sRGB"}
        ($ :feColorMatrix {:in "SourceGraphic" :type "saturate" :values 0.2 :result "Next"})
        ($ :feComponentTransfer {:in "Next"}
          ($ :feFuncR {:type "linear" :slope 0.60})
          ($ :feFuncG {:type "linear" :slope 0.60})
          ($ :feFuncB {:type "linear" :slope 0.60})))
      ($ :rect
        {:id "scene-image-cover"
         :x (.-x (.-a bound))
         :y (.-y (.-a bound))
         :width (seg/width bound)
         :height (seg/height bound)})
      ($ :clipPath {:id "scene-image-clip"}
        ($ :use {:href "#scene-image-cover"})))))

(def ^:private mask-defs-query
  [:user/host
   {:user/camera
    [{:camera/scene
      [[:scene/masked :default false]
       [:scene/lighting :default :revealed]
       {:scene/game-type
        [[:game-type/enabled-elements :default #{}]]}
       {:scene/masks
        [:db/id
         [:mask/vecs :default []]
         [:mask/enabled? :default true]]}
       {:scene/tokens
        [:db/id
         [:object/point :default vec/zero]
         [:object/hidden :default false]
         [:token/light :default 15]]}]}]}])

(defui ^:private mask-defs []
  (let [result (hooks/use-query mask-defs-query)
        {host :user/host
         {{tokens :scene/tokens
           masks  :scene/masks
           lighting :scene/lighting
           masked :scene/masked
           game-type-entity :scene/game-type} :camera/scene} :user/camera} result
        ;; See scene-content's `light` binding -- disabling :unit/light
        ;; overrides the darkness/masking system back to :revealed here too,
        ;; so the two stay in lockstep regardless of which one re-renders.
        light (if (contains? (:game-type/enabled-elements game-type-entity #{}) :unit/light)
                lighting
                :revealed)]
    ($ :defs
      ($ pattern {:id "mask-pattern" :name :crosses})
      ($ :path {:id "masks-path" :d (transduce mask-area-xf poly->path masks)})
      ($ :path {:id "light-path" :d (transduce (token-light-xf host) circle->path tokens)})
      ($ :clipPath {:id "masks-clip"}
        ($ :use {:href "#masks-path"}))
      ($ :mask {:id "masks-mask"}
        ($ :use {:href "#scene-image-cover" :fill "white"})
        ($ :use {:href "#masks-path"}))
      (case [host light masked]
        [true :revealed true]
        ($ :mask {:id "mask-primary"}
          ($ :use {:href "#scene-image-cover" :fill "white"})
          ($ :use {:href "#masks-path"}))
        [true :revealed false]
        ($ :mask {:id "mask-primary"}
          ($ :use {:href "#masks-path" :fill "white"}))
        [true :dimmed true]
        ($ :<>
          ($ :mask {:id "mask-primary"}
            ($ :use {:href "#scene-image-cover" :fill "white"})
            ($ :use {:href "#masks-path"}))
          ($ :mask {:id "mask-secondary"}
            ($ :g {:clip-path "url(#masks-clip)"}
              ($ :use {:href "#scene-image-cover" :fill "white"})
              ($ :use {:href "#masks-path" :fill "rgba(0, 0, 0, 0.5)"})
              ($ :use {:href "#light-path"}))))
        [true :dimmed false]
        ($ :<>
          ($ :mask {:id "mask-primary"}
            ($ :g {:mask "url(#masks-mask)"}
              ($ :use {:href "#scene-image-cover" :fill "white"})
              ($ :use {:href "#light-path"})))
          ($ :mask {:id "mask-secondary"}
            ($ :use {:href "#masks-path" :fill "white"})))
        [true :hidden true]
        ($ :mask {:id "mask-primary"}
          ($ :use {:href "#scene-image-cover" :fill "white"})
          ($ :use {:href "#light-path" :clip-path "url(#masks-clip)"}))
        [true :hidden false]
        ($ :mask {:id "mask-primary"}
          ($ :use {:href "#scene-image-cover" :fill "white"})
          ($ :use {:href "#light-path"})
          ($ :use {:href "#masks-path" :fill "white"}))
        ([false :revealed true] [false :revealed false])
        ($ :clipPath {:id "mask-primary"}
          ($ :use {:href "#masks-path"}))
        [false :dimmed true]
        ($ :<>
          ($ :clipPath {:id "mask-primary"}
            ($ :use {:href "#light-path"}))
          ($ :clipPath {:id "mask-secondary"}
            ($ :use {:href "#masks-path"})))
        [false :dimmed false]
        ($ :<>
          ($ :clipPath {:id "mask-primary"}
            ($ :use {:href "#light-path"}))
          ($ :clipPath {:id "mask-secondary"}
            ($ :use {:href "#masks-path"})))
        [false :hidden true]
        ($ :clipPath {:id "mask-primary" :clip-path "url(#masks-clip)"}
          ($ :use {:href "#light-path"}))
        [false :hidden false]
        ($ :<>
          ($ :clipPath {:id "mask-primary"}
            ($ :use {:href "#light-path"}))
          ($ :clipPath {:id "mask-secondary"}
            ($ :use {:href "#masks-path"})))))))

(def ^:private mask-polys-query
  [[:user/host :default true]
   {:user/camera
    [[:camera/draw-mode :default :select]
     {:camera/scene
      [{:scene/masks
        [:db/id
         [:mask/vecs :default []]
         [:mask/enabled? :default true]]}]}]}])

(defui ^:private mask-polys []
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query mask-polys-query)
        {host :user/host
         {{masks :scene/masks} :camera/scene
          draw-mode :camera/draw-mode} :user/camera} result
        modes #{:mask :mask-toggle :mask-remove}]
    ($ :g.scene-mask-polys {:id "masks-polys"}
      (for [{id :db/id vecs :mask/vecs enabled? :mask/enabled?} masks
            :while (and host (contains? modes draw-mode))]
        ($ :polygon.scene-mask-polygon
          {:key id
           :data-enabled enabled?
           :points (join " " vecs)
           :on-pointer-down stop-propagation
           :on-click
           (fn []
             (case draw-mode
               :mask-toggle (dispatch :mask/toggle id (not enabled?))
               :mask-remove (dispatch :mask/remove id)))})))))

(def ^:private grid-defs-query
  [{:user/camera
    [{:camera/scene
      [[:scene/grid-type :default :square]
       [:scene/grid-shape :default :line]]}]}])

(def ^:private grid-dot-radius 3)

;; A regular octagon's apothem (center-to-edge distance) is its
;; circumradius (center-to-vertex distance) times cos(22.5 deg) -- so to
;; make an octagon's flat edges reach exactly as far as a circle's own
;; radius (i.e. the two shapes share the same bounding box), the octagon
;; needs this much larger a circumradius.
(def ^:private octagon-circumradius-factor (/ 1 (js/Math.cos (/ js/Math.PI 8))))

;; Same idea as `octagon-circumradius-factor`, but for a regular hexagon
;; (apothem = circumradius * cos(30 deg)).
(def ^:private hex-circumradius-factor (/ 1 (js/Math.cos (/ js/Math.PI 6))))

(defn ^:private grid-inscribed-radius
  "The radius of the largest circle that fits entirely inside one cell of
   the given grid-type -- half-size for square-family grids (the cell's
   half-width), or the hex apothem (half of `hex-width`) for hex-family
   grids. Used to size the 'Circles'/'Octo' grid-shape markers so they
   just fit inside the grid space, in contrast to the small fixed-size
   'Dots' position marker."
  [grid-type]
  (if (= (geom/base-grid-type grid-type) :square)
    half-size
    (/ hex-width 2)))

(defn ^:private octagon-points
  "The vertices of a regular octagon of the given circumradius, centered
   on the origin and oriented with flat edges top/bottom/left/right (the
   usual 'clipped-corner square' reading of an octagon)."
  [radius]
  (join " "
        (for [i (range 8)
              :let [a (+ (/ js/Math.PI 8) (* i (/ js/Math.PI 4)))]]
          (str (* radius (js/Math.cos a)) "," (* radius (js/Math.sin a))))))

(defn ^:private hex-marker-points
  "The vertices of a regular hexagon of the given circumradius, centered
   on the origin, matching the actual grid cell's own orientation (pointy-
   or flat-top -- reuses `geom/hex-points`/`hex-points-flat`, the same
   vertex geometry the grid lines themselves are drawn from)."
  [radius flat?]
  (let [points ((if flat? geom/hex-points-flat geom/hex-points) vec/zero radius)]
    (join " " (map (fn [v] (str (.-x v) "," (.-y v))) points))))

(defui ^:private grid-marker
  "Renders a single grid-lattice marker at (cx, cy).

   The small filled 'dot' marker is counter-transformed via the SVG
   presentation `transform` attribute so it stays a true circle regardless
   of the ambient isometric projection -- a fixed-size position marker,
   not board-plane geometry. SVG <pattern> content isn't part of the
   normal layout/formatting tree, so a CSS `transform` (as used elsewhere
   for asset counter-transforms) isn't reliably applied here.

   The larger circle/octagon outline markers are drawn plain, with no
   counter-transform, and simply squish along with the ambient isometric
   projection like the rest of the board (the scene image, tokens, grid
   lines) -- they're meant to preview how much of the grid cell a token
   occupies, so they need to warp the same way that cell does. Countering
   the projection on these (an earlier attempt at this) blew a marker
   already sized to nearly fill its cell up into an oversized ellipse/
   octagon that swallowed neighboring cells once un-warped back to a true
   circle, which is what actually made the isometric grids incomprehensible."
  [{:keys [cx cy grid-type radius shape filled?]}]
  (if filled?
    ($ :g
      {:transform
       (str "translate(" cx ", " cy ")"
            (if (geom/iso? grid-type) (str " " (geom/iso-inverse-matrix grid-type))))}
      ($ :circle.scene-grid-dot {:r radius}))
    ($ :g {:transform (str "translate(" cx ", " cy ")")}
      (case shape
        :octo ($ :polygon.scene-grid-outline {:points (octagon-points radius)})
        :hex  ($ :polygon.scene-grid-outline
                {:points (hex-marker-points radius (= (geom/base-grid-type grid-type) :hex-flat))})
        ($ :circle.scene-grid-outline {:r radius})))))

(defui ^:private grid-defs []
  (let [result (hooks/use-query grid-defs-query)
        {{{grid-type :scene/grid-type
           grid-shape :scene/grid-shape} :camera/scene} :user/camera} result
        base (geom/base-grid-type grid-type)
        ;; "Octo" only makes sense on square-family grids and "Hex" only
        ;; on hex-family grids -- if the scene's stored preference doesn't
        ;; match the grid it's currently on, this falls back to :circle
        ;; instead of getting stuck showing nothing sensible. Like no-grid
        ;; mode and the lighting toggle, this is computed at render time
        ;; rather than destructively overwriting the stored preference, so
        ;; switching back to a matching grid immediately restores it with
        ;; no extra step.
        mode (cond
               (and (= grid-shape :octo) (not= base :square)) :circle
               (and (= grid-shape :hex) (= base :square)) :circle
               :else grid-shape)
        filled? (= mode :dot)
        ;; Circles/Octo/Hex are drawn slightly smaller than the cell they
        ;; mark (70% of the "just fits" size) so they read as a marker
        ;; inside the cell rather than tracing its boundary exactly.
        inscribed (* (grid-inscribed-radius grid-type) 0.7)
        radius (case mode
                 :dot grid-dot-radius
                 :octo (* inscribed octagon-circumradius-factor)
                 :hex (* inscribed hex-circumradius-factor)
                 inscribed)]
    ($ :defs
      (case base
        :hex-pointy
        ($ :pattern
          {:id "grid-pattern"
           :width hex-width
           :height (* 2 hex-row)
           :patternUnits "userSpaceOnUse"}
          (if (= mode :line)
            ($ :path.scene-grid-path {:d geom/hex-pattern-path})
            ($ :<>
              ;; The interior, unclipped hex center for this tile.
              ($ grid-marker {:cx (/ hex-width 2) :cy hex-row :grid-type grid-type :radius radius :shape mode :filled? filled?})
              ;; The other hex center sits on all 4 corners of the tile;
              ;; each corner draws a clipped fragment that, together with
              ;; its counterparts in neighboring tiles, reconstructs one
              ;; full marker per lattice point.
              ($ grid-marker {:cx 0        :cy 0            :grid-type grid-type :radius radius :shape mode :filled? filled?})
              ($ grid-marker {:cx hex-width :cy 0            :grid-type grid-type :radius radius :shape mode :filled? filled?})
              ($ grid-marker {:cx 0        :cy (* 2 hex-row) :grid-type grid-type :radius radius :shape mode :filled? filled?})
              ($ grid-marker {:cx hex-width :cy (* 2 hex-row) :grid-type grid-type :radius radius :shape mode :filled? filled?}))))
        :hex-flat
        ($ :pattern
          {:id "grid-pattern"
           :width (* 2 hex-row)
           :height hex-width
           :patternUnits "userSpaceOnUse"}
          (if (= mode :line)
            ($ :path.scene-grid-path {:d geom/hex-pattern-path-flat})
            ($ :<>
              ($ grid-marker {:cx hex-row :cy (/ hex-width 2) :grid-type grid-type :radius radius :shape mode :filled? filled?})
              ($ grid-marker {:cx 0            :cy 0        :grid-type grid-type :radius radius :shape mode :filled? filled?})
              ($ grid-marker {:cx (* 2 hex-row) :cy 0        :grid-type grid-type :radius radius :shape mode :filled? filled?})
              ($ grid-marker {:cx 0            :cy hex-width :grid-type grid-type :radius radius :shape mode :filled? filled?})
              ($ grid-marker {:cx (* 2 hex-row) :cy hex-width :grid-type grid-type :radius radius :shape mode :filled? filled?}))))
        ($ :pattern
          {:id "grid-pattern"
           :width grid-size
           :height grid-size
           :patternUnits "userSpaceOnUse"}
          (if (= mode :line)
            ($ :path.scene-grid-path {:d (str "M 0 0 H " grid-size " V " grid-size)})
            ;; The token snap center for a square cell sits at the middle
            ;; of the tile, safely clear of any tile boundary.
            ($ grid-marker {:cx half-size :cy half-size :grid-type grid-type :radius radius :shape mode :filled? filled?}))))
      ($ :g {:id "scene-grid"}
        ($ :path
          {:d "M -100000 -100000 H 100000 V 100000 H -100000"
           :fill "url(#grid-pattern)"})))))

(defn ^:private token-flags [data]
  (let [{[{turn :initiative/turn}] :scene/_initiative} data]
    (cond-> (:token/flags data)
      (:scene/_initiative data)       (conj :initiative)
      (= (:db/id data) (:db/id turn)) (conj :turn))))

(defn ^:private token-flags-attr [data]
  (join " " (map name (token-flags data))))

;; Badges ring the token's edge, the first always at its north-west
;; corner and each next one a fixed step counter-clockwise from the last.
;; The step is fixed rather than "spread N badges evenly", so a given
;; condition doesn't jump to a new corner every time an unrelated one is
;; added or cleared -- where a badge sits stays predictable while a fight
;; is in progress.
;;
;; -120 degrees is where the first badge already sat before badges ringed
;; the token at all, kept so existing scenes look unchanged at a glance.
;; Angles run cx=sin/cy=cos against SVG's y-down axis, which makes an
;; INCREASING angle travel counter-clockwise on screen (south -> east ->
;; north -> west).
(def ^:private badge-origin-deg -120)

;; Badges ride the token's outline rather than straddling it. Sitting the
;; ring exactly on `token-radius` centres each badge on the edge, so half
;; of it lies over the token -- and a full ring of eight then buries most
;; of the artwork. Pushing the ring out this far leaves each badge just
;; clipping the outline: the diamond dips 2.5-3.2px inside it (its reach
;; toward the centre varies with angle, since an axis-aligned diamond is
;; an L1 ball) and the circle 2px. Much past this and the circle stops
;; touching the token altogether and reads as floating.
(def ^:private badge-ring-offset 4)

;; An eighth-turn apiece. At the offset ring the eight are comfortable
;; rather than tight -- nearest neighbours clear at |dx|+|dy| = 19.8
;; against the 15 two diamonds need to touch, and 17.6px between centres
;; against the 12 two circles need. (On the un-offset ring the same step
;; was the tightest that still fit, at 16.4 and 14.6.)
(def ^:private badge-step-deg 45)

;; ...which makes 8 the most a token can show. Conditions past it are
;; dropped rather than stacked on top of one another; see token-conditions.
;; The wider ring would seat a ninth, but only at an untidy 40-degree step
;; that shifts every existing badge, so the eighth-turn stays.
(def ^:private badge-limit (quot 360 badge-step-deg))

(defn ^:private token-conditions
  "The flags to badge, in a stable vocabulary order, capped at the
   `badge-limit` that fits around the token. :player and :dead are styled
   on the token body itself rather than badged, so they're excluded here."
  [data icons]
  (let [xform (comp (filter (complement #{:initiative})))
        order (into [:initiative] xform (keys icons))
        exclu #{:player :dead}]
    (take badge-limit (filter (difference (token-flags data) exclu) order))))

(defui ^:private token [{:keys [node data base-scale icons] :or {base-scale 1 icons core-badge-icons}}]
  (let [radius token-radius
        ;; `base-scale` is the scene's per-grid-type "Token scale" setting
        ;; (panel_scene.cljs) -- it multiplies uniformly on top of this
        ;; token's own :token/size, the same way both feed into every use
        ;; of `scale` below (the token's shape, aura anchor, and ring all
        ;; stay proportional to one another regardless of which one moved).
        scale (* (/ (:token/size data) 5) base-scale)
        hash (:image/hash (:image/thumbnail (:token/image data)))
        fill (if (some? hash) (str "token-face-" hash) "token-face-default")]
    ($ :g.scene-token
      {:ref node
       :id (str "token" (:db/id data))
       :data-flags (token-flags-attr data)
       :data-hidden (:object/hidden data)}
      (let [radius (:token/aura-radius data)
            radius (if (> radius 0) (+ (* grid-size (/ radius 5)) (* scale half-size)) 0)]
        ($ :circle.scene-token-aura {:style {:r radius}}))
      ($ :g {:style {:transform (str "scale(" scale ")")}}
        ($ :circle.scene-token-shape {:r radius :fill (str "url(#" fill ")")})
        ($ :circle.scene-token-base {:r (+ radius 5)})
        (for [[idx flag] (map-indexed vector (token-conditions data icons))
              :let [deg (+ badge-origin-deg (* idx badge-step-deg))
                    rn (* (/ js/Math.PI 180) deg)
                    ring (+ radius badge-ring-offset)
                    cx (* (js/Math.sin rn) ring)
                    cy (* (js/Math.cos rn) ring)
                    badge (icons flag)]]
          ($ :g.scene-token-flags {:key flag :data-flag flag :transform (str "translate(" cx ", " cy ")")}
            ;; The diamond is a square on its corner rather than a <polygon>
            ;; so `rx` can round it the way the reference art does. Sized
            ;; for equal area with the default circle (2r^2 vs pi*r^2, so
            ;; a 7.5 half-diagonal against r=6) -- swapping shapes must not
            ;; change how heavy a badge looks against the token.
            (if (= (:shape badge) :diamond)
              ($ :rect
                {:x -5.3 :y -5.3 :width 10.6 :height 10.6 :rx 1.5
                 :transform "rotate(45)"
                 ;; Inline, because a stylesheet `fill` would outrank the
                 ;; same-named presentation attribute; leaving it off lets
                 ;; the CSS default stand for any vocabulary that declares
                 ;; a shape but no per-entry color.
                 :style (if-let [c (:badge-color badge)] {:fill c})})
              ($ :circle {:r 6}))
            ($ :g {:transform (str "translate(" -4 ", " -4 ")")}
              ($ icon {:name (:icon badge) :color (:color badge) :size 8}))))
        (if-let [label (token-label data)]
          ($ :text.scene-token-label {:y half-size} label)))
      (let [radius (+ (* scale half-size) 2)]
        ($ :circle.scene-token-ring {:style {:r radius}})))))

(def ^:private tokens-defs-query
  [[:user/host :default true]
   {:user/camera
    [{:camera/scene
      [[:scene/grid-type :default :square]
       [:scene/token-scale :default {}]
       {:scene/game-type
        [[:game-type/enabled-elements :default #{}]]}
       {:scene/tokens
        [:db/id
         [:initiative/suffix :default nil]
         [:object/point :default vec/zero]
         [:object/hidden :default false]
         [:token/flags :default #{}]
         [:token/label :default ""]
         [:token/size :default 5]
         [:token/light :default 15]
         [:token/aura-radius :default 0]
         {:token/image [{:image/thumbnail [:image/hash]}]}
         {:scene/_initiative [:db/id :initiative/turn]}]}]}]}])

(defui ^:private tokens-defs []
  (let [result (hooks/use-query tokens-defs-query)
        scene  (-> result :user/camera :camera/scene)
        tokens (:scene/tokens scene)
        ;; The base-scale multiplier is tuned per grid-type (see
        ;; panel_scene.cljs's "Token scale" fieldset), so it comes from
        ;; whichever grid-type the scene is currently on.
        base-scale (/ (get (:scene/token-scale scene) (:scene/grid-type scene) 100) 100)
        icons (badge-icons (:game-type/enabled-elements (:scene/game-type scene) #{}))]
    ($ :defs
      ($ :filter {:id "token-status-dead" :filterRes 1 :color-interpolation-filters "sRGB"}
        ($ :feColorMatrix {:in "SourceGraphic" :type "saturate" :values 0 :result "Next"})
        ($ :feComponentTransfer {:in "Next"}
          ($ :feFuncR {:type "linear" :slope 0.70})
          ($ :feFuncG {:type "linear" :slope 0.70})
          ($ :feFuncB {:type "linear" :slope 0.70})))
      ($ :linearGradient {:id "token-base-player" :x1 0 :y1 0 :x2 1 :y2 1}
        ($ :stop {:style {:stop-color "#fcd34d"} :offset "0%"})
        ($ :stop {:style {:stop-color "#b45309"} :offset "100%"}))
      ($ :pattern
        {:id "token-face-default"
         :style {:color "white"}
         :width "100%"
         :height "100%"
         :viewBox "-2 -2 16 16"
         :patternContentUnits "objectBoundingBox"}
        ($ :rect {:x -2 :y -2 :width 16 :height 16 :fill "var(--color-blues-700)"})
        ($ icon {:name "dnd" :size 12}))
      ($ TransitionGroup {:component nil}
        (let [xf (comp (map (comp :image/hash :image/thumbnail :token/image)) (filter some?))]
          (for [hash (into #{} xf tokens)
                :let [node (uix/create-ref)]]
            ($ Transition {:key hash :nodeRef node :timeout 240}
              ($ :pattern
                {:id (str "token-face-" hash)
                 :ref node
                 :width "100%"
                 :height "100%"
                 :patternContentUnits "objectBoundingBox"}
                ($ image {:hash hash}
                  (fn [url]
                    (if (some? url)
                      ($ :image {:href url :width 1 :height 1 :preserveAspectRatio "xMidYMin slice"})
                      ($ :rect {:width 256 :height 256 :fill "var(--color-blues-900)"})))))))))
      ($ TransitionGroup {:component nil}
        (for [{id :db/id :as data} tokens :let [node (uix/create-ref)]]
          ($ Transition {:key id :nodeRef node :timeout 240}
            ($ token {:node node :data data :base-scale base-scale :icons icons})))))))

(def ^:private player-cursors-query
  [{:root/user [{:user/camera [:camera/scene]}]}
   {:root/session
    [[:session/share-cursors :default true]
     {:session/host [:user/uuid]}
     {:session/conns
      [:user/uuid
       [:user/share-cursor :default true]
       [:user/color :default "none"]
       {:user/camera [:camera/scene]}]}]}])

(defui ^:private player-cursors []
  (let [result (hooks/use-query player-cursors-query [:db/ident :root])
        conns  (key-by :user/uuid (:session/conns (:root/session result)))
        share  (:session/share-cursors (:root/session result))
        host   (:session/host (:root/session result))
        [points set-points] (uix/use-state {})]
    (hooks/use-subscribe :cursor/moved
      (uix/use-callback
       (fn [uuid x y]
         (if share
           (set-points (fn [points] (assoc points uuid [x y]))))) [share]))
    (if share
      ($ :g.scene-cursors
        (for [[uuid [x y]] points
              :let [conn (conns uuid)]
              :when (and (some? conn)
                         (:user/share-cursor conn)
                         (= (:camera/scene (:user/camera conn))
                            (:camera/scene (:user/camera (:root/user result)))))]
          ($ hooks/use-portal {:key uuid :name (if (= (:user/uuid host) (:user/uuid conn)) :host-cursor)}
            ($ :g.scene-cursor
              {:transform (str "translate(" (- x 3) ", " (- y 3) ")") :data-color (:user/color conn)}
              ($ icon {:name "cursor-fill" :size 28}))))))))

(defui ^:private scene-camera-draggable
  [{:keys [children]}]
  (let [options (use-draggable #js {"id" "scene-camera" "data" #js {"class" "camera"}})
        dx (getValueByKeys options "transform" "x")
        dy (getValueByKeys options "transform" "y")]
    ($ :g.scene-draggable
      {:id "scene-drag"
       :ref (.-setNodeRef options)
       :style {:will-change "transform"}
       :transform (str "translate(" (or dx 0) ", " (or dy 0) ")")
       :data-dragging (.-isDragging options)
       :on-pointer-down (.. options -listeners -onPointerDown)}
      ($ :rect {:width "100%" :height "100%" :fill "transparent"})
      children)))

(defui ^:private scene-camera
  [{:keys [scale grid-type on-translate children]
    :or   {on-translate identity}}]
  (let [scale-fn (uix/use-memo (fn [] (modifiers/scale-fn scale grid-type)) [scale grid-type])]
    ($ dnd-context
      #js {"modifiers" #js [modifiers/trunc]
           "onDragEnd"
           (uix/use-callback
            (fn [data]
              (let [x (.. data -delta -x)
                    y (.. data -delta -y)]
                (on-translate
                 (Vec2. x y)))) [on-translate])}
      ($ scene-camera-draggable
        ($ dnd-context
          #js {"modifiers" #js [scale-fn modifiers/trunc]}
          children)))))

(def ^:private grid-order-query
  [{:user/camera
    [{:camera/scene
      [[:scene/grid-order-board :default :under]
       [:scene/grid-order-props :default :under]]}]}])

(defn ^:private grid-slot
  "Resolves the single shared grid line's position relative to the board
   and props/tokens bands, given the scene's two independent grid-order
   settings. Board is always the bottommost band and props/tokens are
   always above it, so of the four (board-order, props-order)
   combinations only three distinct positions are actually possible --
   :bottom (grid under both), :middle (grid over board, under props), or
   :top (grid over both). The fourth combination (board :under + props
   :over -- asking for the grid to be simultaneously under the board and
   over the props that are themselves always above the board) has no
   valid single position; board's setting wins in that conflict since it's
   the more fundamental of the two (whether the map is visible through the
   grid at all)."
  [board-order props-order]
  (if (= board-order :over)
    (if (= props-order :over) :top :middle)
    :bottom))

(defui ^:private ^:memo scene-elements []
  (let [result (hooks/use-query grid-order-query)
        {{{grid-order-board :scene/grid-order-board
           grid-order-props :scene/grid-order-props} :camera/scene} :user/camera} result
        ;; The board and props/tokens bands each get their own portal
        ;; target -- `objects` (scene_objects.cljs) mounts once (one
        ;; query, one drag-listener) but teleports its rendered board
        ;; pieces and its rendered props/shapes/notes/tokens into these
        ;; two separate DOM locations, which is what lets the grid render
        ;; independently over/under each one without needing two
        ;; separate mounted instances of the drag machinery.
        board-layer
        ($ hooks/create-portal {:name :board-layer}
          (fn [{:keys [ref]}] ($ :g.scene-board {:ref ref})))
        props-layer
        ($ hooks/create-portal {:name :props-layer}
          (fn [{:keys [ref]}] ($ :g {:ref ref})))]
    ($ :<>
      ;; Defines the standard square grid pattern for the scene.
      ($ grid-defs)

      ;; Defines clip paths and masks for tokens which emit
      ;; radial lighting.
      ($ mask-defs)

      ;; Defines the scene's masking/lighting bounds, derived from the
      ;; union of every placed board piece.
      ($ image-defs)

      ;; Defines token patterns, images, and the tokens themselves.
      ($ tokens-defs)

      ($ :g.scene-exterior
        ($ :use.scene-grid {:href "#scene-grid" :style {:clip-path "unset"}}))

      ;; When the scene is using the "Obscured" lighting option, this
      ;; element becomes visible for players -- just the grid, since
      ;; there's no longer a single scene-wide image to show a
      ;; darkened/desaturated copy of underneath the foreground scene.
      ($ :g.scene-background
        ($ :use.scene-grid {:href "#scene-grid"}))

      ;; The primary scene object contains all interactable objects.
      ;; This element is clipped twice: first by tokens which emit
      ;; light around them, and then second by user-created mask
      ;; areas.
      ($ :g.scene-foreground
        ($ :g.scene-interior
          (case (grid-slot grid-order-board grid-order-props)
            :bottom ($ :<> ($ :use.scene-grid {:href "#scene-grid"}) board-layer props-layer)
            :middle ($ :<> board-layer ($ :use.scene-grid {:href "#scene-grid"}) props-layer)
            :top    ($ :<> board-layer props-layer ($ :use.scene-grid {:href "#scene-grid"})))
          ($ objects))

        ;; Portal target for the host's cursor which must remain obscured
        ;; by visibility controls so that nosey players don't get any
        ;; clues about what the host may be doing on the scene.
        ($ hooks/create-portal {:name :host-cursor}
          (fn [{:keys [ref]}]
            ($ :g {:ref ref :style {:outline "none"} :tab-index -1}))))

      ;; Masking elements that fully or partially obscure the scene.
      ($ :g.scene-mask
        ($ :g.scene-mask-primary
          ($ :use.scene-mask-fill {:href "#scene-image-cover"})
          ($ :use.scene-mask-pattern {:href "#scene-image-cover"})
          ($ :use.scene-grid {:href "#scene-grid"}))
        ($ :g.scene-mask-secondary
          ($ :use.scene-mask-fill {:href "#scene-image-cover"})
          ($ :use.scene-mask-pattern {:href "#scene-image-cover"})
          ($ :use.scene-grid {:href "#scene-grid"})))

    ;; Portal target for selected tokens and shapes. This brings
    ;; selected objects to the foreground so they are not obscured
    ;; by visibility.
    ($ hooks/create-portal {:name :selected}
      (fn [{:keys [ref]}]
        ($ :g {:ref ref :style {:outline "none"} :tab-index -1})))

    ;; Player cursors are rendered on top of everything else.
    ($ player-cursors)

    ;; Renders mask area boundaries, allowing the host to interact
    ;; with them individually.
    ($ mask-polys))))

(def ^:private scene-query
  [:user/host
   {:user/camera
    [:db/id
     [:camera/point :default vec/zero]
     [:camera/scale :default 1]
     [:camera/draw-mode :default :select]
     {:camera/scene
      [:db/id
       [:scene/dark-mode :default false]
       [:scene/show-grid :default true]
       [:scene/grid-type :default :square]
       [:scene/lighting :default :revealed]
       [:scene/masked :default false]
       {:scene/game-type
        [[:game-type/enabled-elements :default #{}]]}]}]}])

(defui ^:private scene-content [props]
  (let [dispatch (hooks/use-dispatch)
        {host :user/host
         {scene :camera/scene
          scale :camera/scale
          mode  :camera/draw-mode
          point :camera/point} :user/camera} (:data props)
        grid-type (:scene/grid-type scene)
        enabled-elements (:game-type/enabled-elements (:scene/game-type scene) #{})
        ;; A game-type with no grid layout enabled overrides "Show grid"
        ;; unconditionally -- the grid is hidden regardless of the
        ;; scene's own stored preference, and that preference is never
        ;; destructively overwritten, so re-enabling a grid layout on the
        ;; game-type makes it reappear without any extra "restore" step.
        has-grid? (pos? (game-type/grid-count enabled-elements))
        ;; Likewise, disabling :unit/light overrides the scene's lighting
        ;; mode back to :revealed unconditionally -- the darkness/masking
        ;; system and per-token light radii are one feature (see
        ;; panel_scene.cljs and scene_context_menu.cljs), and its stored
        ;; :scene/lighting value is never destructively overwritten either.
        light (if (contains? enabled-elements :unit/light) (:scene/lighting scene) :revealed)
        multi-select? (use-key "shift")]
    ($ :svg.scene
      {:ref (:ref props)
       :data-user   (if host "host" "conn")
       :data-grid   (and (not= mode :grid) (:scene/show-grid scene) has-grid?)
       :data-theme  (if (:scene/dark-mode scene) "dark" "light")
       :data-light  (name light)
       :data-masked (:scene/masked scene)}
      ($ :g.scene-handler {:data-type "scene" :tab-index 0 :style {:outline "none"}})
      ($ scene-camera
        {:scale scale
         :grid-type grid-type
         :on-translate
         (uix/use-callback
          (fn [delta]
            (if (= delta vec/zero)
              (dispatch :selection/clear)
              (dispatch :camera/translate (vec/mul delta -1))))
          [dispatch scale])}
        (if (and (= mode :select) multi-select?)
          ($ :g.scene-draw {:data-type "select"}
            ($ draw {:mode :select})))
        ($ :g {:transform
               (str "scale(" scale ") "
                    (if (geom/iso? grid-type) (str (geom/iso-forward-matrix grid-type) " ") "")
                    (vec/rnd (vec/mul point -1)))}
          (:children props)))
      ($ hooks/create-portal {:name :multiselect}
        (fn [{:keys [ref]}]
          ($ :g.scene-draw
            {:ref ref :data-type "select" :tab-index -1})))
      (if (contains? draw-modes mode)
        ($ :g.scene-draw {:data-type (name mode)}
          ($ draw {:key mode :mode mode :node nil}))))))

(defui ^:private scene-transition [props]
  ;; Board pieces are now a per-scene collection rather than one single
  ;; image, so there's no single hash left to gate this transition on
  ;; loading -- it now just fades in on scene switch (by :db/id), without
  ;; waiting for any particular piece's image to finish loading first.
  (let [id (-> props :data :user/camera :camera/scene :db/id)]
    ($ CSSTransition
      {:key (str "id:" id)
       :nodeRef (:ref props)
       :in true
       :timeout 250
       :unmountOnExit true
       :mountOnEnter true
       :appear true}
      (:children props))))

(defui ^:memo scene []
  (let [res (hooks/use-query scene-query)
        ref (uix/use-ref)]
    ($ scene-transition {:ref ref :data res}
      ($ scene-content {:ref ref :data res}
        ($ scene-elements)))))
