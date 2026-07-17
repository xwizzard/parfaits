(ns vec-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [ogres.app.const :refer [hex-radius hex-width hex-row]]
            [ogres.app.vec :as vec :refer [Vec2]]))

(deftest test-nearest-hex
  (testing "a point exactly at a hex center snaps to itself"
    (let [center (Vec2. 0 0)]
      (is (= (vec/nearest-hex center hex-radius) center))))
  (testing "a point near a same-row neighboring hex center snaps to that center"
    (let [center (Vec2. hex-width 0)
          near   (vec/shift center 5 -3)]
      (is (= (vec/nearest-hex near hex-radius) center))))
  (testing "a point near an odd-row (offset) hex center snaps to that center"
    (let [center (Vec2. (/ hex-width 2) hex-row)
          near   (vec/shift center -4 6)]
      (is (= (vec/nearest-hex near hex-radius) center)))))

(deftest test-nearest-hex-flat
  (testing "a point exactly at a hex center snaps to itself"
    (let [center (Vec2. 0 0)]
      (is (= (vec/nearest-hex-flat center hex-radius) center))))
  (testing "a point near a same-column neighboring hex center snaps to that center"
    (let [center (Vec2. 0 hex-width)
          near   (vec/shift center -3 5)]
      (is (= (vec/nearest-hex-flat near hex-radius) center))))
  (testing "a point near an odd-column (offset) hex center snaps to that center"
    (let [center (Vec2. hex-row (/ hex-width 2))
          near   (vec/shift center 6 -4)]
      (is (= (vec/nearest-hex-flat near hex-radius) center))))
  (testing "flat-top snapping is the pointy-top snap transposed"
    (let [point (Vec2. 17 -8)
          flat  (vec/nearest-hex-flat point hex-radius)
          pointy-of-swapped (vec/nearest-hex (Vec2. (.-y point) (.-x point)) hex-radius)]
      (is (= flat (Vec2. (.-y pointy-of-swapped) (.-x pointy-of-swapped)))))))

(deftest test-hex-distance
  (testing "a point measured against itself is zero cells"
    (is (= (vec/hex-distance (Vec2. 0 0) (Vec2. 0 0) hex-radius) 0)))
  (testing "a jittered point in the same hex is still zero cells"
    (let [near (vec/shift (Vec2. 0 0) 5 -3)]
      (is (= (vec/hex-distance (Vec2. 0 0) near hex-radius) 0))))
  (testing "one hex-width apart in the same row is one cell"
    (is (= (vec/hex-distance (Vec2. 0 0) (Vec2. hex-width 0) hex-radius) 1)))
  (testing "an adjacent odd-row-offset neighbor is one cell"
    (is (= (vec/hex-distance (Vec2. 0 0) (Vec2. (/ hex-width 2) hex-row) hex-radius) 1))
    (is (= (vec/hex-distance (Vec2. 0 0) (Vec2. (- (/ hex-width 2)) hex-row) hex-radius) 1)))
  (testing "two rows straight down is two cells"
    (is (= (vec/hex-distance (Vec2. 0 0) (Vec2. 0 (* 2 hex-row)) hex-radius) 2)))
  (testing "a down-right-right hex is two cells"
    (is (= (vec/hex-distance (Vec2. 0 0) (Vec2. (* 1.5 hex-width) hex-row) hex-radius) 2)))
  (testing "a neighbor across a negative row is still one cell"
    (is (= (vec/hex-distance (Vec2. 0 0) (Vec2. (/ hex-width 2) (- (* 1.5 hex-radius))) hex-radius) 1)))
  (testing "distance is symmetric"
    (let [a (Vec2. 10 20) b (Vec2. 300 -150)]
      (is (= (vec/hex-distance a b hex-radius) (vec/hex-distance b a hex-radius))))))
