(ns memory-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [ogres.app.memory :as memory]))

(deftest test-deal
  (let [cards (memory/deal 22 8 10)]
    (testing "44 cards -- 22 values, 2 copies each"
      (is (= (count cards) 44))
      (is (= (frequencies (map :memory/value cards))
             (into {} (map (fn [v] [v 2])) (range 22)))))
    (testing "every card lands at a distinct grid position"
      (is (= (count (into #{} (map :point) cards)) 44)))))

(deftest test-valid-turn-index
  (testing "the given index, when its player is active"
    (is (= (memory/valid-turn-index [:a :b :c] (constantly true) 1) 1)))
  (testing "skips forward past a benched/nonexistent player"
    (is (= (memory/valid-turn-index [:a :b :c] #{:a :c} 1) 2)))
  (testing "wraps around to find the next active player"
    (is (= (memory/valid-turn-index [:a :b :c] #{:a} 1) 0)))
  (testing "nil when no player qualifies"
    (is (nil? (memory/valid-turn-index [:a :b :c] (constantly false) 0))))
  (testing "nil for an empty player list"
    (is (nil? (memory/valid-turn-index [] (constantly true) 0)))))

(deftest test-next-turn-index
  (is (= (memory/next-turn-index [:a :b :c] (constantly true) 0) 1))
  (is (= (memory/next-turn-index [:a :b :c] (constantly true) 1) 2))
  (is (= (memory/next-turn-index [:a :b :c] (constantly true) 2) 0)
      "wraps back to the first player")
  (testing "skips a benched player found while advancing"
    (is (= (memory/next-turn-index [:a :b :c] #{:a :c} 0) 2)))
  (testing "nil when every player is benched/removed"
    (is (nil? (memory/next-turn-index [:a :b :c] (constantly false) 0)))))

(deftest test-winners
  (testing "a single winner"
    (is (= (memory/winners {1 3 2 1 3 0}) #{1})))
  (testing "a tie -- every player at the max score"
    (is (= (memory/winners {1 2 2 2 3 1}) #{1 2})))
  (testing "no scores -- no winners"
    (is (= (memory/winners {}) #{}))))
