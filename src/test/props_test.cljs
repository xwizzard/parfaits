(ns props-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [ogres.app.props :as props]))

(deftest test-grid-offset
  (testing "cell 0 is the origin"
    (is (= (props/grid-offset 0 8 10) [0 0])))
  (testing "row-major -- fills columns before wrapping to the next row"
    (is (= (props/grid-offset 1 8 10) [10 0]))
    (is (= (props/grid-offset 7 8 10) [70 0]))
    (is (= (props/grid-offset 8 8 10) [0 10]))
    (is (= (props/grid-offset 9 8 10) [10 10]))))

(def ^:private a {:db/id 1 :object/variables {:pile/id "x" :pile/position 0}})
(def ^:private b {:db/id 2 :object/variables {:pile/id "x" :pile/position 2}})
(def ^:private c {:db/id 3 :object/variables {:pile/id "x" :pile/position 1}})
(def ^:private d {:db/id 4 :object/variables {:pile/id "y" :pile/position 0}})
(def ^:private e {:db/id 5}) ; no :object/variables at all

(deftest test-in-pile?
  (is (true? (props/in-pile? "x" a)))
  (is (false? (props/in-pile? "x" d)))
  (is (false? (props/in-pile? "x" e))))

(deftest test-pile
  (is (= (set (map :db/id (props/pile [a b c d e] "x"))) #{1 2 3})
      "only members of the given pile-id, other piles and un-piled entities excluded"))

(deftest test-top-of-pile
  (is (= (:db/id (props/top-of-pile [a b c])) 2)
      "the member with the highest :pile/position")
  (is (nil? (props/top-of-pile []))))

(deftest test-next-pile-position
  (is (= (props/next-pile-position [a b c]) 3)
      "one past the current max")
  (is (= (props/next-pile-position []) 0)
      "the first position in a brand-new pile"))
