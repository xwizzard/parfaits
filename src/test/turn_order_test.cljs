(ns turn-order-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [ogres.app.turn-order :as turn-order]))

(deftest test-valid-turn-index
  (testing "the given index, when its player is active"
    (is (= (turn-order/valid-turn-index [:a :b :c] (constantly true) 1) 1)))
  (testing "skips forward past a benched/nonexistent player"
    (is (= (turn-order/valid-turn-index [:a :b :c] #{:a :c} 1) 2)))
  (testing "wraps around to find the next active player"
    (is (= (turn-order/valid-turn-index [:a :b :c] #{:a} 1) 0)))
  (testing "nil when no player qualifies"
    (is (nil? (turn-order/valid-turn-index [:a :b :c] (constantly false) 0))))
  (testing "nil for an empty player list"
    (is (nil? (turn-order/valid-turn-index [] (constantly true) 0)))))

(deftest test-next-turn-index
  (is (= (turn-order/next-turn-index [:a :b :c] (constantly true) 0) 1))
  (is (= (turn-order/next-turn-index [:a :b :c] (constantly true) 1) 2))
  (is (= (turn-order/next-turn-index [:a :b :c] (constantly true) 2) 0)
      "wraps back to the first player")
  (testing "skips a benched player found while advancing"
    (is (= (turn-order/next-turn-index [:a :b :c] #{:a :c} 0) 2)))
  (testing "nil when every player is benched/removed"
    (is (nil? (turn-order/next-turn-index [:a :b :c] (constantly false) 0)))))

(deftest test-winners
  (testing "a single winner"
    (is (= (turn-order/winners {1 3 2 1 3 0}) #{1})))
  (testing "a tie -- every player at the max score"
    (is (= (turn-order/winners {1 2 2 2 3 1}) #{1 2})))
  (testing "no scores -- no winners"
    (is (= (turn-order/winners {}) #{}))))
