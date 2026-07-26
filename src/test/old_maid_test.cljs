(ns old-maid-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [ogres.app.old-maid :as old-maid]))

(deftest test-pairs-to-discard
  (testing "a clean pair"
    (let [hand [{:db/id 1 :card/rank :two} {:db/id 2 :card/rank :two}]]
      (is (= (set (map :db/id (old-maid/pairs-to-discard hand))) #{1 2}))))
  (testing "an unmatched single card is never included"
    (let [hand [{:db/id 1 :card/rank :two}]]
      (is (empty? (old-maid/pairs-to-discard hand)))))
  (testing "the Old Maid card (a unique rank, no mate) never matches"
    (let [hand [{:db/id 1 :card/rank :queen :card/label "Old Maid"}]]
      (is (empty? (old-maid/pairs-to-discard hand)))))
  (testing "a 3-of-a-kind discards exactly 1 pair, leaving the odd card"
    (let [hand [{:db/id 1 :card/rank :king} {:db/id 2 :card/rank :king} {:db/id 3 :card/rank :king}]]
      (is (= (count (old-maid/pairs-to-discard hand)) 2))))
  (testing "a 4-of-a-kind discards as 2 separate pairs, all 4 cards"
    (let [hand [{:db/id 1 :card/rank :ace} {:db/id 2 :card/rank :ace}
                {:db/id 3 :card/rank :ace} {:db/id 4 :card/rank :ace}]]
      (is (= (set (map :db/id (old-maid/pairs-to-discard hand))) #{1 2 3 4}))))
  (testing "a mixed hand -- one complete pair, one single, one triple"
    (let [hand [{:db/id 1 :card/rank :two} {:db/id 2 :card/rank :two}
                {:db/id 3 :card/rank :three}
                {:db/id 4 :card/rank :four} {:db/id 5 :card/rank :four} {:db/id 6 :card/rank :four}]]
      (is (= (set (map :db/id (old-maid/pairs-to-discard hand))) #{1 2 4 5})
          "the :two pair and 2 of the 3 :four cards discard; the lone
           :three and the odd :four stay")))
  (testing "an empty hand"
    (is (empty? (old-maid/pairs-to-discard [])))))
