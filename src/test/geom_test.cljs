(ns geom-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [ogres.app.geom :as geom]
            [ogres.app.matrix :as matrix]
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
