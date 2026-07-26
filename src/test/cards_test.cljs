(ns cards-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [ogres.app.cards :as cards]))

(deftest test-shuffle-positions
  (testing "assigns every id a distinct position covering exactly 0..n-1"
    (let [ids [:a :b :c :d :e]
          positions (cards/shuffle-positions ids)]
      (is (= (set (keys positions)) (set ids)))
      (is (= (set (vals positions)) (set (range (count ids)))))))
  (testing "different calls produce different orders"
    (let [ids (range 20)
          a (cards/shuffle-positions ids)
          b (cards/shuffle-positions ids)]
      (is (not= a b)
          "vanishingly unlikely to collide for 20 items if truly shuffled"))))

(deftest test-rank-value
  (testing "ace resolves high or low depending on the ruleset -- nothing else does"
    (is (= (cards/rank-value :ace {:ace-high? true}) 14))
    (is (= (cards/rank-value :ace {:ace-high? false}) 1))
    (is (= (cards/rank-value :king {:ace-high? true}) (cards/rank-value :king {:ace-high? false}))
        "face cards and numbers never change value based on the ace ruleset"))
  (testing "numeric ranks resolve to their printed number"
    (is (= (cards/rank-value :two {}) 2))
    (is (= (cards/rank-value :ten {}) 10)))
  (testing "face cards resolve above all numbers, in jack < queen < king < ace order"
    (is (< (cards/rank-value :ten {})
           (cards/rank-value :jack {})
           (cards/rank-value :queen {})
           (cards/rank-value :king {})
           (cards/rank-value :ace {}))))
  (testing "an undefined rank (e.g. a joker) has no numeric value"
    (is (nil? (cards/rank-value :joker {})))))

(deftest test-cards-of-rank
  (let [cards [{:db/id 1 :card/rank :two} {:db/id 2 :card/rank :three} {:db/id 3 :card/rank :two}]]
    (is (= (set (map :db/id (cards/cards-of-rank cards :two))) #{1 3}))
    (is (empty? (cards/cards-of-rank cards :nine)))))

(deftest test-cards-of-holder
  (let [cards [{:db/id 1 :card/location :hand :card/holder {:db/id 10}}
               {:db/id 2 :card/location :hand :card/holder {:db/id 20}}
               {:db/id 3 :card/location :hand :card/holder {:db/id 10}}
               {:db/id 4 :card/location :scored :card/holder {:db/id 10}}]]
    (is (= (set (map :db/id (cards/cards-of-holder cards 10))) #{1 3}))
    (is (empty? (cards/cards-of-holder cards 99)))
    (is (not (contains? (set (map :db/id (cards/cards-of-holder cards 10))) 4))
        "a card credited to holder-id but no longer :card/location :hand
         (e.g. already scored) isn't still counted as being in their hand")))

(deftest test-needs-reshuffle?
  (is (true? (cards/needs-reshuffle? [] [{:card/rank :two}]))
      "empty draw, non-empty discard -- reclaim it")
  (is (false? (cards/needs-reshuffle? [{:card/rank :two}] []))
      "draw still has cards -- no need to reclaim anything")
  (is (false? (cards/needs-reshuffle? [] []))
      "both empty -- nothing to reclaim"))
