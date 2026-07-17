(ns ogres.app.vec
  (:refer-clojure :exclude [abs max mod map]))

(def ^:private rad->deg (/ 180 js/Math.PI))
(def ^:private tau (* 2 js/Math.PI))

(defn ^:private to-string [x y]
  (str "#vec2[" x "," y "]"))

(defprotocol IVec2
  (abs [a])
  (add [a b])
  (angle [a])
  (dist [a] [a b])
  (dist-cheb [a] [a b])
  (div [a x])
  (heading [a])
  (max [a])
  (mod [a x])
  (mul [a x])
  (normalize [a])
  (rnd [a] [a x] [a x f])
  (shift [a n] [a x y])
  (sub [a b])
  (transform [a m]))

(deftype Vec2 [x y]
  Object
  (toString [_]
    (str "translate(" x ", " y ")"))
  cljs.core/IPrintWithWriter
  (-pr-writer [_ writer _]
    (-write writer (to-string x y)))
  IEquiv
  (-equiv [_ v]
    (and (instance? Vec2 v) (= (.-x v) x) (= (.-y v) y)))
  IHash
  (-hash [_]
    (hash [x y]))
  ISeqable
  (-seq [_]
    (list x y))
  IVec2
  (abs [_]
    (Vec2. (clojure.core/abs x) (clojure.core/abs y)))
  (add [_ b]
    (Vec2. (+ x (.-x b)) (+ y (.-y b))))
  (angle [_]
    (let [rad (js/Math.atan2 y x)]
      (if (neg? rad)
        (* rad->deg (+ rad tau))
        (* rad->deg rad))))
  (dist [_]
    (js/Math.hypot x y))
  (dist [a b]
    (dist (sub b a)))
  (dist-cheb [a]
    (max (abs a)))
  (dist-cheb [a b]
    (max (abs (sub a b))))
  (div [_ n]
    (Vec2. (/ x n) (/ y n)))
  (heading [_]
    (js/Math.atan2 y x))
  (normalize [a]
    (let [m (dist a)]
      (if (zero? m) a
          (div a m))))
  (max [_]
    (clojure.core/max x y))
  (mod [_ n]
    (Vec2. (clojure.core/mod x n) (clojure.core/mod y n)))
  (mul [_ n]
    (Vec2. (* x n) (* y n)))
  (rnd [_]
    (Vec2. (js/Math.round x) (js/Math.round y)))
  (rnd [_ n]
    (Vec2. (* (js/Math.round (/ x n)) n) (* (js/Math.round (/ y n)) n)))
  (rnd [_ n f]
    (Vec2. (* (f (/ x n)) n) (* (f (/ y n)) n)))
  (shift [_ n]
    (Vec2. (+ x n) (+ y n)))
  (shift [_ n m]
    (Vec2. (+ x n) (+ y m)))
  (sub [_ b]
    (Vec2. (- x (.-x b)) (- y (.-y b))))
  (transform [_ ^Matrix m]
    (let [t (.matrixTransform (js/DOMPointReadOnly. x y) (.-m m))]
      (Vec2. (.-x t) (.-y t)))))

(def zero (Vec2. 0 0))

(defn nearest-square
  "Given a point and the size of one grid square, returns the center of
   the nearest square cell as a Vec2 -- the square-grid analogue of
   `nearest-hex`. Rounding a point directly to the nearest multiple of
   `size` (i.e. plain `rnd`) lands on a grid-line intersection -- a cell
   CORNER, not a center. Shifting by half a cell before rounding, then
   back by the same half-cell after, is what actually lands on the
   CENTER of the nearest cell (this is the same shift-round-shift
   formula the codebase already used inline for token placement before
   this function existed to name it and make it reusable)."
  [point size]
  (let [half (/ size 2)
        x (- (.-x point) half)
        y (- (.-y point) half)]
    (Vec2. (+ (* (js/Math.round (/ x size)) size) half)
           (+ (* (js/Math.round (/ y size)) size) half))))

(defn ^:private nearest-hex-index
  "Given a point and a hexagon radius, returns the [row col] offset
   coordinate of the nearest pointy-top hexagon in the odd-r lattice
   `nearest-hex` (below) snaps to -- odd rows offset horizontally by half
   a hexagon's width, rows spaced `1.5 * radius` apart vertically.
   Factored out as its own function (rather than post-processing
   `nearest-hex`'s pixel-space output) because a hex center in pixel
   space can't be reliably inverted back into exact [row col] indices due
   to float rounding -- this candidate search is the one place that
   actually knows the winning indices along the way. Both `nearest-hex`
   and `hex-distance` build on it. Ported from the brute-force
   nearest-center search in worldhaven-asset-browser/public/builder.js."
  [point radius]
  (let [x (.-x point)
        y (.-y point)
        hex-w (* (js/Math.sqrt 3) radius)
        row-h (* 1.5 radius)
        approx-row (js/Math.round (/ y row-h))
        candidates
        (for [r (range (- approx-row 1) (+ approx-row 2))
              :let [x-off (if (zero? (clojure.core/mod r 2)) 0 (/ hex-w 2))
                    cy (* r row-h)
                    approx-col (js/Math.round (/ (- x x-off) hex-w))]
              c (range (- approx-col 1) (+ approx-col 2))
              :let [cx (+ (* c hex-w) x-off)
                    dx (- cx x)
                    dy (- cy y)]]
          [[r c] (+ (* dx dx) (* dy dy))])]
    (first (apply min-key second candidates))))

(defn nearest-hex
  "Given a point and a hexagon radius (the distance from center to a
   vertex), returns the center of the nearest pointy-top hexagon as a Vec2.
   Hexagons tile in rows spaced `1.5 * radius` apart vertically; odd rows
   are offset horizontally by half a hexagon's width. This matches the
   layout of real Gloomhaven/Frosthaven board tiles, and is the hex
   analogue of `rnd` for the square grid."
  [point radius]
  (let [[r c] (nearest-hex-index point radius)
        hex-w (* (js/Math.sqrt 3) radius)
        row-h (* 1.5 radius)
        x-off (if (zero? (clojure.core/mod r 2)) 0 (/ hex-w 2))]
    (Vec2. (+ (* c hex-w) x-off) (* r row-h))))

(defn hex-distance
  "The number of hex-grid steps between two points on a pointy-top hex
   grid of the given radius -- i.e. how many cells apart they are, not a
   continuous pixel distance. The hex analogue of `dist-cheb`. Snaps each
   point to its nearest hex (the same odd-r lattice `nearest-hex-index`
   snaps to), converts each [row col] offset coordinate to axial
   (`q = col - (row - (mod row 2)) / 2`, `r = row` -- the standard
   'odd-r' offset-to-axial conversion for a lattice whose odd rows are
   shifted right), then axial to cube (`x = q, z = r, y = -x - z`), and
   returns the cube distance `(|Δx| + |Δy| + |Δz|) / 2` -- the standard
   hex-grid-distance formula. Always an integer (cube coordinates always
   sum to zero, so their absolute differences always sum to an even
   number). Like `nearest-hex`, this snaps to a lattice centered at world
   origin, so it's not translation-invariant the way `dist-cheb` is --
   consistent with the rest of this codebase's hex-snap assumptions (e.g.
   token placement)."
  [a b radius]
  (letfn [(axial [[row col]] [(- col (/ (- row (clojure.core/mod row 2)) 2)) row])
          (cube [[q r]] [q (- (- q) r) r])]
    (let [[xa ya za] (cube (axial (nearest-hex-index a radius)))
          [xb yb zb] (cube (axial (nearest-hex-index b radius)))]
      (/ (+ (clojure.core/abs (- xa xb))
            (clojure.core/abs (- ya yb))
            (clojure.core/abs (- za zb)))
         2))))

(defn nearest-hex-flat
  "Given a point and a hexagon radius, returns the center of the nearest
   flat-top hexagon as a Vec2. A flat-top hex grid is a pointy-top hex
   grid rotated 90 degrees: hexagons tile in offset columns spaced
   `1.5 * radius` apart horizontally instead of offset rows. Rather than
   re-deriving the geometry, this swaps x and y, delegates to `nearest-hex`
   (which handles the offset-lattice math), then swaps back -- valid
   because transposing both axes of a honeycomb yields another honeycomb
   with the same edge structure, just rotated."
  [point radius]
  (let [swapped (nearest-hex (Vec2. (.-y point) (.-x point)) radius)]
    (Vec2. (.-y swapped) (.-x swapped))))
