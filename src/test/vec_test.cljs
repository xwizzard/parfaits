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
