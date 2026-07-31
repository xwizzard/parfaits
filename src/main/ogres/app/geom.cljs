(ns ogres.app.geom
  (:require [clojure.math :refer [floor ceil]]
            [ogres.app.const :refer [grid-size half-size hex-radius hex-width hex-row]]
            [ogres.app.matrix :as matrix]
            [ogres.app.memory :as memory]
            [ogres.app.segment :as seg :refer [Segment]]
            [ogres.app.vec :as vec :refer [Vec2]]))

(def ^:const deg45->rad (/ js/Math.PI 4))
(def ^:const deg45->sin (js/Math.sin deg45->rad))

(def ^:private iso-square-forward
  "The fixed isometric projection for square grids: rotate 45 degrees, then
   scale the Y axis by half (the classic 2:1 dimetric diamond look). Has no
   translation component, which is what allows the same substitution to
   work for both absolute points and pure deltas everywhere it's used."
  (matrix/from-coeffs deg45->sin (* 0.5 deg45->sin)
                       (- deg45->sin) (* 0.5 deg45->sin)
                       0 0))

(def ^:private iso-square-inverse
  (matrix/inverse iso-square-forward))

(def ^:private iso-hex-forward
  "The fixed isometric projection for hex grids: just a vertical squish (Y
   scaled by half), with NO rotation -- rotating 45 degrees doesn't align
   with a hexagon's 6-fold symmetry the way it does with a square's 4-fold
   symmetry, so hex grids are simply squished flat rather than turned into
   a diamond."
  (matrix/from-coeffs 1 0 0 0.5 0 0))

(def ^:private iso-hex-inverse
  (matrix/inverse iso-hex-forward))

(def ^:private iso-square-vertical-forward
  "A 90-degree-rotated 'vertical' variant of `iso-square-forward` -- rarely
   used, but some scenarios read better as a portrait diamond instead of
   the classic landscape one. Composed from the existing forward matrix
   rather than re-derived by hand, so it inherits the same correctness."
  (matrix/multiply (matrix/rotate matrix/identity 90) iso-square-forward))

(def ^:private iso-square-vertical-inverse
  (matrix/inverse iso-square-vertical-forward))

(def ^:private iso-hex-vertical-forward
  "A 90-degree-rotated 'vertical' variant of `iso-hex-forward` -- squishes
   the X axis instead of Y, giving a tall/narrow hex instead of a
   wide/flat one."
  (matrix/multiply (matrix/rotate matrix/identity 90) iso-hex-forward))

(def ^:private iso-hex-vertical-inverse
  (matrix/inverse iso-hex-vertical-forward))

(def ^:private iso-grid-types
  #{:iso-square :iso-hex-pointy :iso-hex-flat
    :iso-square-vertical :iso-hex-pointy-vertical :iso-hex-flat-vertical})

(defn iso?
  "True if the given :scene/grid-type is one of the isometric variants."
  [grid-type]
  (contains? iso-grid-types grid-type))

(defn base-grid-type
  "Maps an isometric grid-type to the base grid-type whose pattern/snapping
   logic it reuses (e.g. :iso-hex-pointy -> :hex-pointy). Non-iso grid-types
   are returned unchanged."
  [grid-type]
  (case grid-type
    (:iso-square :iso-square-vertical) :square
    (:iso-hex-pointy :iso-hex-pointy-vertical) :hex-pointy
    (:iso-hex-flat :iso-hex-flat-vertical) :hex-flat
    grid-type))

(defn cell-distance
  "The number of grid cells between the two ends of `segment` -- the hex
   analogue of `dist-cheb`/`px->ft`'s Chebyshev-pixel-distance convention,
   but counting whole cells instead of a real-world unit. `segment` is
   expected to already be in scene/logical coordinates (e.g.
   scene_draw.cljs's `camera` value) -- the isometric projection, if any,
   is already inverted out of a segment by the time it reaches here (see
   `scene-scale-matrix`, composed into the basis matrix every drawing
   tool builds its `camera` segment from, *before* any tool sees it), so
   this function never needs to know or care whether `grid-type` is an
   iso variant, only its base shape.

   Square grids (and their iso variants) count cells via Chebyshev pixel
   distance divided by `grid-size`, matching the diagonal-costs-the-same
   convention `px->ft` already assumes. Hex grids (and their iso
   variants) use `vec/hex-distance` -- flat-top hex grids transpose x/y
   first, the same trick `vec/nearest-hex-flat` already uses to reuse the
   pointy-top lattice math for a flat-top grid (hex distance is
   rotation-invariant, so there's no need to transpose the result back)."
  [segment grid-type]
  (case (base-grid-type grid-type)
    :hex-pointy (vec/hex-distance (.-a segment) (.-b segment) hex-radius)
    :hex-flat   (let [flip (fn [p] (Vec2. (.-y p) (.-x p)))]
                  (vec/hex-distance (flip (.-a segment)) (flip (.-b segment)) hex-radius))
    (js/Math.round (/ (vec/dist-cheb segment) grid-size))))

(defn iso-forward-matrix
  "The fixed isometric projection Matrix for the given grid-type, or nil if
   grid-type isn't an iso variant."
  [grid-type]
  (case grid-type
    :iso-square iso-square-forward
    :iso-square-vertical iso-square-vertical-forward
    (:iso-hex-pointy :iso-hex-flat) iso-hex-forward
    (:iso-hex-pointy-vertical :iso-hex-flat-vertical) iso-hex-vertical-forward
    nil))

(defn iso-inverse-matrix
  "The inverse of `iso-forward-matrix` for the given grid-type, or nil if
   grid-type isn't an iso variant. Computed by DOMMatrix itself rather than
   by hand -- used to convert a screen-space point/delta on an iso scene
   back into the scene's pre-projection logical coordinates."
  [grid-type]
  (case grid-type
    :iso-square iso-square-inverse
    :iso-square-vertical iso-square-vertical-inverse
    (:iso-hex-pointy :iso-hex-flat) iso-hex-inverse
    (:iso-hex-pointy-vertical :iso-hex-flat-vertical) iso-hex-vertical-inverse
    nil))

(defn iso-inverse
  "Applies the isometric inverse projection to v when grid-type is an iso
   variant; otherwise returns v unchanged."
  [grid-type v]
  (if-let [m (iso-inverse-matrix grid-type)] (m v) v))

(defn screen->scene-vec
  "Converts a screen-space vector (an absolute point already relative to
   the viewport origin, or a pure delta) into scene-space units, accounting
   for camera scale and, for iso grid-types, the fixed isometric
   projection. Safe for both points and deltas since the projection has no
   translation component."
  [v scale grid-type]
  (iso-inverse grid-type (vec/div v (or scale 1))))

(defn scene-scale-matrix
  "The Matrix equivalent of `screen->scene-vec`'s scaling step, for
   composing into existing matrix/translate chains (e.g. :props/create,
   the scene_draw drawing tools) in place of a plain `matrix/scale`."
  [scale grid-type]
  (let [s (matrix/scale matrix/identity (/ 1 (or scale 1)))]
    (if-let [m (iso-inverse-matrix grid-type)] (matrix/multiply m s) s)))

(defn clockwise-triangle?
  [a b c]
  (> (- (* (- (.-x b) (.-x a)) (- (.-y c) (.-y a)))
        (* (- (.-y b) (.-y a)) (- (.-x c) (.-x a)))) 0))

(defn point-within-rect?
  [point segment]
  (and (< (.-x (.-a segment)) (.-x point) (.-x (.-b segment)))
       (< (.-y (.-a segment)) (.-y point) (.-y (.-b segment)))))

(defn point-within-circle?
  [point center radius]
  (< (vec/dist center point) radius))

(defn point-within-triangle?
  [a b c v]
  (and (clockwise-triangle? a b v)
       (clockwise-triangle? c a v)
       (clockwise-triangle? b c v)))

(defn line-points
  ([segment]
   (line-points segment half-size))
  ([segment width]
   (let [ln width
         av (.-a segment)
         bv (.-b segment)
         ax (.-x (.-a segment))
         ay (.-y (.-a segment))
         bx (.-x (.-b segment))
         by (.-y (.-b segment))]
     (if (= ay by)
       ;; `(- ln)`, not `(- ay ln)`: shift's 3-arity adds, so subtracting
       ;; `ln` FROM `ay` put these two corners at `y = by + ay - ln`
       ;; instead of `ay - ln`. That happens to be right only when
       ;; ay = 0 -- which is exactly what the one masking call site
       ;; passes (a Segment from vec/zero), so the band came out correct
       ;; there and wrong for every other horizontal line, including in
       ;; object-bounding-rect, where the line then fell OUTSIDE its own
       ;; bounding rect.
       [(vec/shift av 0 ln)
        (vec/shift bv 0 ln)
        (vec/shift bv 0 (- ln))
        (vec/shift av 0 (- ln))]
       (let [ma (/ (- bx ax) (- ay by))
             mb (js/Math.sqrt (inc (* ma ma)))
             si (js/Math.sign (- ay by))
             dv (Vec2. (* ln si (/ mb)) (* ln si (/ ma mb)))]
         [(vec/add av (vec/mul dv -1))
          (vec/add bv (vec/mul dv -1))
          (vec/add bv dv)
          (vec/add av dv)])))))

(defn cone-points [segment]
  (let [src (.-a segment)
        dst (.-b segment)
        alt (vec/dist src dst)
        hyp (js/Math.hypot alt (/ alt 2))
        rad (vec/heading (vec/sub dst src))
        ax (* hyp (js/Math.cos (- rad 0.46)))
        ay (* hyp (js/Math.sin (- rad 0.46)))
        bx (* hyp (js/Math.cos (+ rad 0.46)))
        by (* hyp (js/Math.sin (+ rad 0.46)))]
    [src (vec/add src (Vec2. ax ay)) (vec/add src (Vec2. bx by))]))

(def hex-pattern-path
  "An SVG path `d` string for one repeating tile of a pointy-top hexagonal
   grid, sized to `hex-radius`. Intended as the content of an SVG <pattern>
   whose width is `hex-width` and height is `(* 2 hex-row)` (one full
   vertical period of the offset-row hex layout, matching real
   Gloomhaven/Frosthaven board tiles).

   Each hexagon contributes only its upper-left diagonal, upper-right
   diagonal, and right vertical edge; the remaining three edges (left
   vertical, bottom-left diagonal, bottom-right diagonal) are supplied by
   neighboring hexes' own copies of this same drawing once the pattern
   tiles, so no edge is ever stroked twice. Candidates are enumerated with
   a generous margin (extra rows/columns whose geometry falls entirely
   outside the tile are harmless — they contribute no visible pixels)."
  (apply str
         (for [row (range 3)
               col (range -1 3)
               :let [x-off (if (odd? row) (/ hex-width 2) 0)
                     cx (+ (* col hex-width) x-off)
                     cy (* row hex-row)
                     v5x (- cx (/ hex-width 2)) v5y (- cy (/ hex-radius 2))
                     v0x cx                     v0y (- cy hex-radius)
                     v1x (+ cx (/ hex-width 2)) v1y (- cy (/ hex-radius 2))
                     v2x (+ cx (/ hex-width 2)) v2y (+ cy (/ hex-radius 2))]]
           (str "M" v5x "," v5y
                "L" v0x "," v0y
                "L" v1x "," v1y
                "L" v2x "," v2y " "))))

(def hex-pattern-path-flat
  "An SVG path `d` string for one repeating tile of a flat-top hexagonal
   grid, sized to `hex-radius`. Intended as the content of an SVG <pattern>
   whose width is `(* 2 hex-row)` and height is `hex-width` -- the
   transpose of `hex-pattern-path`'s tile.

   A flat-top hex grid is a pointy-top hex grid rotated 90 degrees:
   hexagons tile in offset columns instead of offset rows. This reuses the
   exact same candidate generation as `hex-pattern-path` (same edge
   ownership: upper-left diagonal, upper-right diagonal, right vertical)
   but swaps x and y in the emitted coordinates, which is a valid tiling
   because transposing both axes of a honeycomb preserves which edges
   belong to which hexagon -- it just rotates the whole pattern."
  (apply str
         (for [row (range 3)
               col (range -1 3)
               :let [x-off (if (odd? row) (/ hex-width 2) 0)
                     cx (+ (* col hex-width) x-off)
                     cy (* row hex-row)
                     v5x (- cx (/ hex-width 2)) v5y (- cy (/ hex-radius 2))
                     v0x cx                     v0y (- cy hex-radius)
                     v1x (+ cx (/ hex-width 2)) v1y (- cy (/ hex-radius 2))
                     v2x (+ cx (/ hex-width 2)) v2y (+ cy (/ hex-radius 2))]]
           (str "M" v5y "," v5x
                "L" v0y "," v0x
                "L" v1y "," v1x
                "L" v2y "," v2x " "))))

(defn hex-points
  "Returns the 6 vertices of a pointy-top hexagon centered at `center`
   with the given radius, as a vector of Vec2s, suitable for an SVG
   <polygon points=\"...\">. Vertex angles match `hex-pattern-path`."
  [center radius]
  (vec (for [i (range 6)
             :let [rad (* (/ js/Math.PI 180) (- (* 60 i) 90))]]
         (vec/add center (Vec2. (* radius (js/Math.cos rad)) (* radius (js/Math.sin rad)))))))

(defn hex-points-flat
  "Returns the 6 vertices of a flat-top hexagon centered at `center` with
   the given radius, as a vector of Vec2s -- the transpose of
   `hex-points`, matching how `hex-pattern-path-flat` is derived from
   `hex-pattern-path`."
  [center radius]
  (vec (for [i (range 6)
             :let [rad (* (/ js/Math.PI 180) (- (* 60 i) 90))]]
         (vec/add center (Vec2. (* radius (js/Math.sin rad)) (* radius (js/Math.cos rad)))))))

(defn tile-points [point]
  [point
   (vec/shift point grid-size 0)
   (vec/shift point grid-size)
   (vec/shift point 0 grid-size)])

(defn rect-points [segment]
  [(.-a segment)
   (Vec2. (.-x (.-b segment)) (.-y (.-a segment)))
   (Vec2. (.-x (.-a segment)) (.-y (.-b segment)))
   (.-b segment)])

(defn rect-intersects-rect [a b]
  (not (or (< (.-x (.-b a)) (.-x (.-a b)))
           (< (.-x (.-b b)) (.-x (.-a a)))
           (< (.-y (.-b a)) (.-y (.-a b)))
           (< (.-y (.-b b)) (.-y (.-a a))))))

(defn bounding-rect-rf
  ([] seg/zero)
  ([s] s)
  ([s v]
   (if (identical? s seg/zero)
     (Segment. v v)
     (Segment.
      (Vec2. (min (.-x (.-a s)) (.-x v)) (min (.-y (.-a s)) (.-y v)))
      (Vec2. (max (.-x (.-b s)) (.-x v)) (max (.-y (.-b s)) (.-y v)))))))

(defn bounding-rect
  [points]
  (reduce bounding-rect-rf (bounding-rect-rf) points))

(defn clockwise?
  [[ax ay :as xs]]
  (loop [[bx by cx cy :as xs] xs sum 0]
    (if (some? cx)
      (recur (rest (rest xs)) (+ (* (- cx bx) (+ cy by)) sum))
      (neg? (+ (* (- ax bx) (+ ay by)) sum)))))

(defn reorient
  [xs]
  (if (clockwise? xs) xs
      (into [] cat (reverse (partition 2 xs)))))

(defn tile-edge [a b c]
  (cond (= (.-y c) (.-y a)) 0
        (= (.-x c) (.-x b)) 1
        (= (.-y c) (.-y b)) 2
        (= (.-x c) (.-x a)) 3))

(def tile-edge-path
  [(Segment. (Vec2.  1 0) (Vec2. 0 1))
   (Segment. (Vec2.  0 1) (Vec2. -1 0))
   (Segment. (Vec2. -1 0) (Vec2. 0 -1))
   (Segment. (Vec2. 0 -1) (Vec2. 1 0))])

(defn path-around-tiles
  [points]
  (if (not (seq points)) []
      (let [corners (into [] (mapcat tile-points) points)
            visited (into #{} corners)
            bounds (bounding-rect corners)
            src (.-a bounds)
            dst (.-b bounds)
            start (first (filter (fn [v] (= (.-y v) (.-y src))) visited))]
        (loop [n start rs (transient []) ed 0]
          (if (and (= n start) (> (count rs) 0)) (persistent! rs)
              (let [s (tile-edge-path ed)
                    c (vec/add (vec/mul (.-a s) grid-size) n)]
                (if (contains? visited c)
                  (recur c (conj! rs c) (or (tile-edge src dst c) ed))
                  (let [d (vec/add (vec/mul (.-b s) grid-size) n)]
                    (recur d (conj! rs d) (or (tile-edge src dst d) ed))))))))))

(defn tile-path-circle
  [center radius]
  (let [sz grid-size
        hs half-size
        ln (* radius deg45->sin)
        av (vec/rnd (vec/shift center (- radius)) sz floor)
        bv (vec/rnd (vec/shift center radius) sz ceil)
        cv (vec/rnd (vec/shift center (- ln)) sz ceil)
        dv (vec/rnd (vec/shift center ln) sz floor)]
    (loop [x (.-x av) y (.-y av) rs (transient [])]
      (let [t (Vec2. x y)]
        (cond (> x (.-x bv)) (path-around-tiles (persistent! rs))
              (> y (.-y bv)) (recur (+ x sz) (.-y av) rs)
              (and (= y (.-y cv)) (>= x (.-x cv)) (< x (.-x dv))
                   (not (= cv dv))
                   (not (= cv t))
                   (not (and (= x (.-x cv)) (= y (.-y dv))))
                   (not (and (= x (- (.-x dv) sz)) (= y (.-y cv))))
                   (not (and (= x (- (.-x dv) sz)) (= y (.-y dv)))))
              (recur x (.-y dv) rs)
              (point-within-circle? (vec/shift t hs) center radius)
              (recur x (+ y sz) (conj! rs t))
              :else
              (recur x (+ y sz) rs))))))

(defn tile-path-cone
  [[a b c :as xs]]
  (let [sz grid-size
        rt (bounding-rect xs)
        tl (vec/rnd (.-a rt) sz floor)
        br (vec/rnd (.-b rt) sz ceil)]
    (loop [x (.-x tl) y (.-y tl) rs (transient [])]
      (let [t (Vec2. x y)]
        (cond (> x (.-x br)) (path-around-tiles (persistent! rs))
              (> y (.-y br))
              (recur (+ x sz) (.-y tl) rs)
              (or (point-within-triangle? a b c (vec/shift t 14 14))
                  (point-within-triangle? a b c (vec/shift t 56 14))
                  (point-within-triangle? a b c (vec/shift t 14 56))
                  (point-within-triangle? a b c (vec/shift t 56 56)))
              (recur x (+ y sz) (conj! rs t))
              :else
              (recur x (+ y sz) rs))))))

(defn tile-path-line
  [[a b c d :as xs]]
  (let [sz grid-size
        rt (bounding-rect xs)
        tl (vec/rnd (.-a rt) sz floor)
        br (vec/rnd (.-b rt) sz ceil)]
    (loop [x (.-x tl) y (.-y tl) rs (transient [])]
      (let [t (vec/shift (Vec2. x y) half-size)]
        (cond (> x (.-x br)) (path-around-tiles (persistent! rs))
              (> y (.-y br)) (recur (+ x sz) (.-y tl) rs)
              (or (point-within-triangle? a b c t)
                  (point-within-triangle? a c d t))
              (recur x (+ y sz) (conj! rs (Vec2. x y)))
              :else
              (recur x (+ y sz) rs))))))

(defmulti object-bounding-rect
  :object/type)

;; Tokens are defined by their position {A} and size.
(defmethod object-bounding-rect :token/token
  [{src :object/point size :token/size}]
  (let [rad (/ (* (or size 5) grid-size) 10)]
    (Segment. (vec/shift src (- rad)) (vec/shift src rad))))

;; A mini-game table is defined by its origin {A} and the unscaled size
;; of the card grid it draws, scaled by :object/scale. The cards
;; themselves hold no coordinates -- see ogres.app.memory/card-offset --
;; which is exactly what makes the whole board one movable, scalable
;; object rather than N independent props.
(defmethod object-bounding-rect :minigame/table
  [{src :object/point scale :object/scale}]
  (let [[w h] (memory/table-footprint)
        s (or scale 1)
        mid (Vec2. (/ w 2) (/ h 2))
        arm (Vec2. (* (/ w 2) s) (* (/ h 2) s))
        ctr (vec/add src mid)]
    (Segment. (vec/sub ctr arm) (vec/add ctr arm))))

;; Circles are defined by points {A, B} where A is the center and B is
;; some point on the circumference.
(defmethod object-bounding-rect :shape/circle
  [{src :object/point [dst] :shape/points}]
  (let [rad (vec/dist-cheb dst)]
    (Segment. (vec/shift src (- rad)) (vec/shift src rad))))

;; Cones are isosceles triangles defined by points {A, B} where A is
;; the apex and B is the center of the base.
(defmethod object-bounding-rect :shape/cone
  [{src :object/point [dst] :shape/points}]
  (let [dst (vec/add dst src)]
    (bounding-rect (cone-points (Segment. src dst)))))

;; Rectangles are defined by points {A, B} where A and B are opposite and
;; opposing corners, such as top-left and bottom-right.
(defmethod object-bounding-rect :shape/rect
  [{src :object/point [dst] :shape/points}]
  (let [dst (vec/add dst src)]
    (bounding-rect (list src dst))))

;; Polygons are defined by points {A, B, C, [...]} where each point is
;; adjacent to its neighbors.
(defmethod object-bounding-rect :shape/poly
  [{src :object/point points :shape/points}]
  (let [xfr (map (fn [v] (vec/add src v)))]
    (bounding-rect (list* src (sequence xfr points)))))

;; Lines are defined by points {A, B}, opposite ends of the segment.
(defmethod object-bounding-rect :shape/line
  [{src :object/point [dst] :shape/points}]
  (let [dst (vec/add dst src)]
    (bounding-rect (line-points (Segment. src dst)))))

;; Notes are defined by the point {A} and is fixed square bound.
(defmethod object-bounding-rect :note/note
  [{src :object/point}]
  (Segment. src (vec/shift src 42)))

(defmethod object-bounding-rect :prop/prop
  [{point :object/point
    scale :object/scale
    rotation :object/rotation
    {width :image/width
     height :image/height} :prop/image}]
  (let [bound (Segment. point (vec/shift point width height))
        ;; (or scale 1)/(or rotation 0) as object-transform already does:
        ;; DOMMatrix.scale takes an unrestricted double, so a nil scale
        ;; coerces to 0 rather than defaulting to 1 and collapses the
        ;; whole rect to a point at the image's centre. Reachable from the
        ;; context menu's "reset transformations", which retracts both.
        xform (-> matrix/identity
                  (matrix/translate (seg/midpoint bound))
                  (matrix/scale (or scale 1))
                  (matrix/rotate (or rotation 0))
                  (matrix/translate (vec/mul (seg/midpoint bound) -1)))]
    (bounding-rect (map xform (rect-points bound)))))

;; Board pieces are defined exactly like props (point, scale, rotation,
;; an image with a natural width/height) -- same rotated/scaled bounding
;; box math, just reading :board/image instead of :prop/image.
(defmethod object-bounding-rect :board/piece
  [{point :object/point
    scale :object/scale
    rotation :object/rotation
    {width :image/width
     height :image/height} :board/image}]
  (let [bound (Segment. point (vec/shift point width height))
        ;; (or scale 1)/(or rotation 0) as object-transform already does:
        ;; DOMMatrix.scale takes an unrestricted double, so a nil scale
        ;; coerces to 0 rather than defaulting to 1 and collapses the
        ;; whole rect to a point at the image's centre. Reachable from the
        ;; context menu's "reset transformations", which retracts both.
        xform (-> matrix/identity
                  (matrix/translate (seg/midpoint bound))
                  (matrix/scale (or scale 1))
                  (matrix/rotate (or rotation 0))
                  (matrix/translate (vec/mul (seg/midpoint bound) -1)))]
    (bounding-rect (map xform (rect-points bound)))))

(defn ^:private image-anchor-point
  "The world-space position of the given image-bearing entity's grid
   anchor -- the point within its own artwork that should snap to a grid
   cell center, which for irregular/jigsaw-shaped tile art isn't
   necessarily the image's bounding-box center. Falls back to the
   bounding-box center when no :image/anchor has been calibrated (see
   :image/set-anchor), so this is a pure generalization of the old
   center-only behavior, not a change for uncalibrated images."
  [{point :object/point scale :object/scale rotation :object/rotation} image]
  (let [{width :image/width height :image/height anchor :image/anchor} image
        bound (Segment. point (vec/shift point width height))
        center (seg/midpoint bound)
        local (vec/add point (or anchor (Vec2. (/ width 2) (/ height 2))))
        xform (-> matrix/identity
                  (matrix/translate center)
                  (matrix/scale (or scale 1))
                  (matrix/rotate (or rotation 0))
                  (matrix/translate (vec/mul center -1)))]
    (xform local)))

(defmulti object-anchor-point
  "The entity's true grid-alignment point -- what actually gets snapped to
   the nearest cell center by snap-to-cell. Defaults to the entity's own
   bounding-rect midpoint (today's behavior for every type). Props and
   board pieces override this to account for a per-image :image/anchor
   calibration, since the artwork's own hex/square grid doesn't
   necessarily center on the image's bounding box."
  :object/type)

(defmethod object-anchor-point :default
  [entity]
  (seg/midpoint (object-bounding-rect entity)))

(defmethod object-anchor-point :prop/prop
  [entity]
  (image-anchor-point entity (:prop/image entity)))

(defmethod object-anchor-point :board/piece
  [entity]
  (image-anchor-point entity (:board/image entity)))

(defmulti object-transform :object/type)

(defmethod object-transform :default []
  matrix/identity)

(defmethod object-transform :prop/prop
  [{scale :object/scale rotation :object/rotation
    {width :image/width height :image/height} :prop/image}]
  (let [bounds (Segment. vec/zero (Vec2. width height))
        center (seg/midpoint bounds)]
    (-> (matrix/translate matrix/identity center)
        (matrix/scale (or scale 1))
        (matrix/rotate (or rotation 0))
        (matrix/translate (vec/mul center -1)))))

;; A table scales about its middle, like every other scalable object --
;; which is also what lets the generic corner handles work on it: all four
;; corners sit the same distance from the center, so one radius ratio is
;; the scale factor no matter which corner is dragged. Anchoring at the
;; origin instead would put a handle exactly on the anchor and divide by
;; zero the moment it moved.
(defmethod object-transform :minigame/table
  [{scale :object/scale}]
  (let [[w h] (memory/table-footprint)
        center (seg/midpoint (Segment. vec/zero (Vec2. w h)))]
    (-> (matrix/translate matrix/identity center)
        (matrix/scale (or scale 1))
        (matrix/translate (vec/mul center -1)))))

(defmethod object-transform :board/piece
  [{scale :object/scale rotation :object/rotation
    {width :image/width height :image/height} :board/image}]
  (let [bounds (Segment. vec/zero (Vec2. width height))
        center (seg/midpoint bounds)]
    (-> (matrix/translate matrix/identity center)
        (matrix/scale (or scale 1))
        (matrix/rotate (or rotation 0))
        (matrix/translate (vec/mul center -1)))))

(defmulti object-tile-path
  (fn [object _ _]
    (:object/type object)))

(defmethod object-tile-path :default [] [])

(defmethod object-tile-path :shape/circle
  [{src :object/point [dst] :shape/points} delta]
  (let [src (vec/add src delta)
        dst (vec/add dst src)
        rad (vec/dist-cheb src dst)]
    (tile-path-circle src rad)))

(defmethod object-tile-path :shape/cone
  [{src :object/point [dst] :shape/points} delta]
  (let [src (vec/add src delta)
        dst (vec/add dst src)]
    (tile-path-cone (cone-points (Segment. src dst)))))

(defmethod object-tile-path :shape/line
  [{src :object/point [dst] :shape/points} delta]
  (let [src (vec/add src delta)
        dst (vec/add dst src)]
    (tile-path-line (line-points (Segment. src dst)))))

(defn object-alignment [entity]
  (case (:object/type entity)
    :shape/cone half-size
    :shape/line half-size
    :shape/poly half-size
    grid-size))

(defn snap-to-cell
  "Snaps the given entity's center to the nearest grid-cell center for the
   given base grid-type family, after being shifted by delta -- returns the
   entity's own :object/point that produces that snapped center. Works for
   any object type that has an object-anchor-point method, not just tokens.

   For tokens, :object/point already *is* the center (object-anchor-point's
   :default case is the bounding-box midpoint, and object-bounding-rect
   :token/token is symmetric around :object/point), so this degenerates to
   exactly the existing token hex/square snap math -- this is a
   generalization, not a behavior change, for tokens. For props and board
   pieces, whose :object/point is the unrotated/unscaled top-left corner,
   this is what makes 'snap to grid' mean 'snap this piece's true anchor
   point (its calibrated grid center, or the bounding-box center by
   default -- see object-anchor-point) to the nearest cell.'

   All three branches snap the same `moved` value (the anchor point plus
   delta) directly -- `vec/nearest-hex`/`vec/nearest-hex-flat`/
   `vec/nearest-square` all return the nearest CELL CENTER for their grid
   family, not a corner. An earlier version of the square-grid branch
   instead rounded the raw bounding-box corners and took their midpoint,
   ignoring the calibrated anchor point entirely -- which lands on a grid
   CORNER rather than a center whenever the footprint is an even number of
   cells across, regardless of where the calibrated anchor actually was.
   That is not a tokens-are-safe/props-are-not distinction, as an earlier
   version of this note claimed: a token's footprint is `:token/size` * 14
   px, so every even size the context menu offers (10/20/30/40/50, i.e.
   2/4/6/8/10 cells) is affected too. It survived that long only because
   the corner-rounding version was left in the drag PREVIEW rather than
   the committed value -- see component/scene-objects' object-hint, which
   now derives its preview from this function instead."
  [entity delta base-type]
  (let [point (:object/point entity)
        center (object-anchor-point entity)
        moved (vec/add center delta)
        snapped (case base-type
                  :hex-pointy (vec/nearest-hex moved hex-radius)
                  :hex-flat (vec/nearest-hex-flat moved hex-radius)
                  (vec/nearest-square moved grid-size))]
    (vec/add point (vec/sub snapped center))))
