(ns geom-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [ogres.app.const :refer [grid-size hex-radius hex-width hex-row]]
            [ogres.app.geom :as geom]
            [ogres.app.matrix :as matrix]
            [ogres.app.segment :refer [Segment]]
            [ogres.app.vec :as vec :refer [Vec2]]))

(defn ^:private close?
  "True if a and b are within a small floating-point tolerance of each
   other -- DOMMatrix inversion involves division, so exact equality isn't
   a safe assertion for a forward/inverse round trip."
  [a b]
  (< (vec/dist a b) 0.001))

(def ^:private all-iso-grid-types
  [:iso-square :iso-hex-pointy :iso-hex-flat
   :iso-square-vertical :iso-hex-pointy-vertical :iso-hex-flat-vertical])

(deftest test-iso-matrix-round-trip
  (doseq [grid-type all-iso-grid-types
          :let [forward (geom/iso-forward-matrix grid-type)
                inverse (geom/iso-inverse-matrix grid-type)]]
    (testing (str grid-type ": the isometric inverse undoes the forward transform")
      (doseq [v [(Vec2. 0 0) (Vec2. 70 0) (Vec2. 0 70) (Vec2. 35 105) (Vec2. -140 63)]]
        (is (close? (inverse (forward v)) v)
            (str "round trip failed for " v))))
    (testing (str grid-type ": the isometric projection has no translation component")
      (is (close? (forward (Vec2. 0 0)) (Vec2. 0 0)))
      (is (close? (inverse (Vec2. 0 0)) (Vec2. 0 0))))))

(deftest test-iso-square-rotates-and-squishes
  (testing "the square projection rotates 45 degrees (off-diagonal terms are non-zero)"
    (let [forward (geom/iso-forward-matrix :iso-square)
          v (Vec2. 100 0)
          result (forward v)]
      ;; A point on the pure X axis should end up with a non-zero Y after a
      ;; 45-degree rotation -- if this were ever "just squished" (no
      ;; rotation), result's Y would stay 0.
      (is (not (zero? (.-y result)))
          "expected the square iso projection to rotate, not just squish"))))

(deftest test-iso-hex-only-squishes
  (testing "the hex projection is a pure vertical squish, with no rotation"
    (let [forward (geom/iso-forward-matrix :iso-hex-pointy)
          v (Vec2. 100 0)
          result (forward v)]
      ;; A point on the pure X axis must stay on the X axis (Y unchanged at
      ;; 0) if the projection is squish-only with no rotation.
      (is (close? result (Vec2. 100 0))
          "expected the hex iso projection to leave the X axis untouched")))
  (testing "hex-pointy and hex-flat share the same (squish-only) projection"
    (is (identical? (geom/iso-forward-matrix :iso-hex-pointy)
                    (geom/iso-forward-matrix :iso-hex-flat)))
    (is (identical? (geom/iso-forward-matrix :iso-hex-pointy-vertical)
                    (geom/iso-forward-matrix :iso-hex-flat-vertical)))))

(deftest test-iso-vertical-is-90-degree-rotation
  (let [rotate90 (matrix/rotate matrix/identity 90)]
    (doseq [[normal vertical] [[:iso-square :iso-square-vertical]
                               [:iso-hex-pointy :iso-hex-pointy-vertical]]]
      (testing (str vertical " is exactly a 90-degree rotation of " normal)
        (doseq [v [(Vec2. 100 0) (Vec2. 0 100) (Vec2. 42 -17) (Vec2. 0 0)]]
          (is (close? ((geom/iso-forward-matrix vertical) v)
                      (rotate90 ((geom/iso-forward-matrix normal) v)))
              (str "mismatch for " v)))))))

(deftest test-iso-and-base-grid-type
  (testing "iso? recognizes only the isometric variants"
    (doseq [grid-type all-iso-grid-types]
      (is (geom/iso? grid-type) (str grid-type " should be iso?")))
    (is (not (geom/iso? :square)))
    (is (not (geom/iso? :hex-pointy)))
    (is (not (geom/iso? :hex-flat))))
  (testing "base-grid-type maps iso variants (including vertical) to their base"
    (is (= (geom/base-grid-type :iso-square) :square))
    (is (= (geom/base-grid-type :iso-square-vertical) :square))
    (is (= (geom/base-grid-type :iso-hex-pointy) :hex-pointy))
    (is (= (geom/base-grid-type :iso-hex-pointy-vertical) :hex-pointy))
    (is (= (geom/base-grid-type :iso-hex-flat) :hex-flat))
    (is (= (geom/base-grid-type :iso-hex-flat-vertical) :hex-flat))
    (is (= (geom/base-grid-type :square) :square))
    (is (= (geom/base-grid-type :hex-pointy) :hex-pointy))))

(deftest test-snap-to-cell-anchor-point
  (testing "a board piece's calibrated anchor snaps to a true cell CENTER on a
            square grid, not a grid-line corner -- regression test for a bug
            where the square branch ignored the calibrated anchor entirely
            and rounded the raw (often even-cell-wide) image bounding box
            instead, which only coincidentally lands on a center for
            symmetric odd-cell-wide tokens"
    (let [;; A 700x700px image (exactly 10 grid cells, an EVEN count) with
          ;; its calibrated anchor at the image's own top-left corner
          ;; (local (0,0), deliberately off the image's geometric center)
          ;; -- this is exactly the shape that triggered the bug.
          entity {:object/type :board/piece
                  :object/point (Vec2. 0 0)
                  :object/scale 1
                  :object/rotation 0
                  :board/image {:image/width 700 :image/height 700
                                :image/anchor (Vec2. 0 0)}}
          result (geom/snap-to-cell entity vec/zero :square)]
      ;; The anchor (at local (0,0)) ends up at object/point + (0,0), so the
      ;; snapped object/point IS the anchor's new world position.
      (is (= result (Vec2. (/ grid-size 2) (/ grid-size 2)))
          "Snaps to (35,35) -- a true cell center (an odd multiple of
           half-grid-size) -- not (0,0) or any other grid-line corner.")
      (is (not (zero? (clojure.core/mod (.-x result) grid-size)))
          "Explicitly not a corner: corners are exact multiples of grid-size.")))
  (testing "an off-center, non-zero anchor still snaps correctly"
    (let [entity {:object/type :board/piece
                  :object/point (Vec2. 0 0)
                  :object/scale 1
                  :object/rotation 0
                  :board/image {:image/width 700 :image/height 700
                                :image/anchor (Vec2. 100 150)}}
          result (geom/snap-to-cell entity vec/zero :square)
          new-anchor-world (vec/add result (Vec2. 100 150))]
      (is (= new-anchor-world (vec/nearest-square (Vec2. 100 150) grid-size))
          "The calibrated anchor point itself (not the image's geometric
           center) is what lands on the nearest cell center.")))
  (testing "hex grids were already correct -- unaffected by this fix"
    (let [entity {:object/type :board/piece
                  :object/point (Vec2. 0 0)
                  :object/scale 1
                  :object/rotation 0
                  :board/image {:image/width 700 :image/height 700
                                :image/anchor (Vec2. 0 0)}}
          result (geom/snap-to-cell entity vec/zero :hex-pointy)]
      (is (= result (vec/nearest-hex (Vec2. 0 0) hex-radius))))))

(deftest test-cell-distance
  (testing "square grids count cells via grid-size-normalized Chebyshev distance"
    (is (= (geom/cell-distance (Segment. (Vec2. 0 0) (Vec2. grid-size 0)) :square) 1))
    (is (= (geom/cell-distance (Segment. (Vec2. 0 0) (Vec2. grid-size grid-size)) :square) 1)
        "a diagonal grid-size step costs the same as an orthogonal one, matching
         px->ft's existing diagonal-costs-the-same convention")
    (is (= (geom/cell-distance (Segment. (Vec2. 0 0) (Vec2. (* 3 grid-size) 0)) :square) 3)))
  (testing "hex-pointy delegates to vec/hex-distance directly"
    (is (= (geom/cell-distance (Segment. (Vec2. 0 0) (Vec2. hex-width 0)) :hex-pointy)
           (vec/hex-distance (Vec2. 0 0) (Vec2. hex-width 0) hex-radius))))
  (testing "hex-flat transposes x/y before delegating -- a same-row pointy-top
            neighbor is a same-column flat-top neighbor"
    (is (= (geom/cell-distance (Segment. (Vec2. 0 0) (Vec2. 0 hex-width)) :hex-flat) 1)))
  (testing "iso variants produce identical results to their base grid-type for the
            same (already-logical-space) segment -- cell-distance never needs to
            know or care whether grid-type is an iso variant"
    (let [square-seg (Segment. (Vec2. 0 0) (Vec2. (* 2 grid-size) grid-size))
          hex-seg    (Segment. (Vec2. 0 0) (Vec2. hex-width hex-row))]
      (is (= (geom/cell-distance square-seg :square)
             (geom/cell-distance square-seg :iso-square)
             (geom/cell-distance square-seg :iso-square-vertical)))
      (is (= (geom/cell-distance hex-seg :hex-pointy)
             (geom/cell-distance hex-seg :iso-hex-pointy)
             (geom/cell-distance hex-seg :iso-hex-pointy-vertical))))))

(deftest test-screen->scene-vec
  (let [v (Vec2. 140 70)]
    (testing "non-iso grid-types are a plain scale division"
      (is (= (geom/screen->scene-vec v 2 :square) (vec/div v 2)))
      (is (= (geom/screen->scene-vec v 2 :hex-pointy) (vec/div v 2))))
    (testing "iso grid-types additionally apply the isometric inverse"
      (is (= (geom/screen->scene-vec v 2 :iso-square)
             ((geom/iso-inverse-matrix :iso-square) (vec/div v 2))))
      (is (= (geom/screen->scene-vec v 2 :iso-hex-flat)
             ((geom/iso-inverse-matrix :iso-hex-flat) (vec/div v 2))))
      (is (= (geom/screen->scene-vec v 2 :iso-square-vertical)
             ((geom/iso-inverse-matrix :iso-square-vertical) (vec/div v 2))))
      (is (= (geom/screen->scene-vec v 2 :iso-hex-flat-vertical)
             ((geom/iso-inverse-matrix :iso-hex-flat-vertical) (vec/div v 2)))))))

(defn ^:private y-range
  "The [min max] y of a quad's corners."
  [pts]
  (let [ys (map #(.-y %) pts)]
    [(apply min ys) (apply max ys)]))

(deftest test-line-points-band-is-centred-on-the-line
  (testing "line-points returns a quad `width` to either side of the
            segment. The horizontal branch is a separate special case, so
            it needs pinning independently -- it used to add `ay` into the
            two far corners, which only cancels at ay = 0 (exactly what
            the one masking call site passes, hiding it there)."
    (let [w 35]
      (testing "horizontal at y = 0 -- the case that always worked"
        (is (= (y-range (geom/line-points (Segment. (Vec2. 0 0) (Vec2. 200 0)) w))
               [-35 35])))

      (testing "horizontal away from the origin"
        (is (= (y-range (geom/line-points (Segment. (Vec2. 0 100) (Vec2. 200 100)) w))
               [65 135])
            "a 70-wide band centred on y=100, not a 30-wide band below it")
        (is (= (y-range (geom/line-points (Segment. (Vec2. 0 -70) (Vec2. 200 -70)) w))
               [-105 -35])
            "and correct for negative y too"))

      (testing "the horizontal branch agrees with the general branch in the limit"
        (let [exact (y-range (geom/line-points (Segment. (Vec2. 0 100) (Vec2. 200 100)) w))
              near  (y-range (geom/line-points (Segment. (Vec2. 0 100) (Vec2. 200 100.0001)) w))]
          (is (< (abs (- (first exact) (first near))) 0.01))
          (is (< (abs (- (second exact) (second near))) 0.01))))

      (testing "a horizontal line lies inside its own bounding rect"
        ;; this is what selection, viewport culling and paste bounds rely on
        (let [seg (Segment. (Vec2. 0 100) (Vec2. 200 100))
              rect (geom/bounding-rect (geom/line-points seg w))]
          (is (<= (.-y (.-a rect)) 100 (.-y (.-b rect)))))))))
