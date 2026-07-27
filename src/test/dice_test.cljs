(ns dice-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [ogres.app.dice :as dice]))

(deftest test-roll-die-bounds
  (testing "every roll of a d4 lands in [1, 4]"
    (is (every? #(<= 1 % 4) (repeatedly 200 #(dice/roll-die 4)))))
  (testing "every roll of a d20 lands in [1, 20]"
    (is (every? #(<= 1 % 20) (repeatedly 200 #(dice/roll-die 20)))))
  (testing "every roll of a d100 lands in [1, 100]"
    (is (every? #(<= 1 % 100) (repeatedly 200 #(dice/roll-die 100)))))
  (testing "a d1 always rolls 1"
    (is (every? #(= 1 %) (repeatedly 20 #(dice/roll-die 1))))))

(deftest test-roll-dice
  (let [rolled (dice/roll-dice [20 6 6 4])]
    (is (= (count rolled) 4) "one result per side-count given, same count")
    (is (= (map :sides rolled) [20 6 6 4]) "same order as the input, sides preserved on each result")
    (is (every? (fn [{:keys [sides value]}] (<= 1 value sides)) rolled)
        "every individual result is within its own die's bounds"))
  (is (empty? (dice/roll-dice [])) "an empty pool rolls nothing"))

(deftest test-sum
  (is (= (dice/sum [{:sides 6 :value 3} {:sides 6 :value 5} {:sides 20 :value 11}]) 19)
      "the total of every rolled die's value, regardless of size")
  (is (= (dice/sum [{:sides 4 :value 4}]) 4) "a single die sums to its own value"))

(deftest test-best
  (is (= (dice/best [{:sides 20 :value 8} {:sides 20 :value 14}]) {:sides 20 :value 14})
      "advantage's combining rule -- the single highest-value die, ties broken arbitrarily")
  (is (= (dice/best [{:sides 20 :value 14} {:sides 20 :value 14}]) {:sides 20 :value 14})
      "a tie for highest still returns a card with that value"))

(deftest test-worst
  (is (= (dice/worst [{:sides 20 :value 8} {:sides 20 :value 14}]) {:sides 20 :value 8})
      "disadvantage's combining rule -- the single lowest-value die")
  (is (= (:value (dice/worst [{:sides 12 :value 3} {:sides 6 :value 3}])) 3)
      "mixed die sizes are compared purely by rolled value, not sides -- which
       tied entry comes back is arbitrary, only the value is guaranteed"))
