(ns go-fish-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [ogres.app.go-fish :as go-fish]))

(deftest test-cards-of-rank
  (let [cards [{:db/id 1 :card/rank :two} {:db/id 2 :card/rank :three} {:db/id 3 :card/rank :two}]]
    (is (= (set (map :db/id (go-fish/cards-of-rank cards :two))) #{1 3}))
    (is (empty? (go-fish/cards-of-rank cards :nine)))))

(deftest test-scoreable-count
  (testing "book mode -- all-or-nothing"
    (is (= (go-fish/scoreable-count 4 true) 4) "exactly 4 -- a complete book")
    (is (= (go-fish/scoreable-count 3 true) 0) "not yet eligible")
    (is (= (go-fish/scoreable-count 2 true) 0))
    (is (= (go-fish/scoreable-count 0 true) 0)))
  (testing "pair mode -- lays down 2 at a time, no need to wait for 4"
    (is (= (go-fish/scoreable-count 2 false) 2) "exactly one pair")
    (is (= (go-fish/scoreable-count 3 false) 2)
        "a 3-of-a-kind lays down 1 pair, keeping the odd card in hand")
    (is (= (go-fish/scoreable-count 4 false) 4) "two full pairs at once")
    (is (= (go-fish/scoreable-count 1 false) 0) "a single card is never eligible")
    (is (= (go-fish/scoreable-count 0 false) 0))))
