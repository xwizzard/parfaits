(ns attack-deck-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [ogres.app.attack-deck :as attack-deck]))

(deftest test-standard-composition
  (is (= (apply + (vals attack-deck/standard-composition)) 20)
      "the standard deck is always 20 cards")
  (is (= (:minus-2 attack-deck/standard-composition) 1))
  (is (= (:minus-1 attack-deck/standard-composition) 5))
  (is (= (:plus-0 attack-deck/standard-composition) 6))
  (is (= (:plus-1 attack-deck/standard-composition) 5))
  (is (= (:plus-2 attack-deck/standard-composition) 1))
  (is (= (:null attack-deck/standard-composition) 1))
  (is (= (:times-2 attack-deck/standard-composition) 1)))

(deftest test-value
  (is (= (attack-deck/value :minus-2) -2))
  (is (= (attack-deck/value :minus-1) -1))
  (is (= (attack-deck/value :plus-0) 0))
  (is (= (attack-deck/value :plus-1) 1))
  (is (= (attack-deck/value :plus-2) 2))
  (testing "Null/Curse are always the worst possible outcome, not just a low number"
    (is (< (attack-deck/value :null) (attack-deck/value :minus-2)))
    (is (< (attack-deck/value :curse) (attack-deck/value :minus-2)))
    (is (= (attack-deck/value :null) (attack-deck/value :curse))))
  (testing "2x/Bless are always the best possible outcome, not just a high number"
    (is (> (attack-deck/value :times-2) (attack-deck/value :plus-2)))
    (is (> (attack-deck/value :bless) (attack-deck/value :plus-2)))
    (is (= (attack-deck/value :times-2) (attack-deck/value :bless)))))

(deftest test-shuffle-triggering?
  (is (attack-deck/shuffle-triggering? :null))
  (is (attack-deck/shuffle-triggering? :times-2))
  (is (not (attack-deck/shuffle-triggering? :bless))
      "BLESS is one-shot removed, not a shuffle-icon card")
  (is (not (attack-deck/shuffle-triggering? :curse)))
  (is (not (attack-deck/shuffle-triggering? :plus-1))))

(deftest test-removed-on-draw?
  (is (attack-deck/removed-on-draw? :bless))
  (is (attack-deck/removed-on-draw? :curse))
  (is (not (attack-deck/removed-on-draw? :null)))
  (is (not (attack-deck/removed-on-draw? :times-2)))
  (is (not (attack-deck/removed-on-draw? :plus-0))))

(deftest test-better
  (is (= (attack-deck/better :plus-1 :minus-1) :plus-1)
      "advantage keeps the numerically higher card")
  (is (= (attack-deck/better :minus-1 :plus-1) :plus-1))
  (is (= (attack-deck/better :plus-2 :times-2) :times-2)
      "2x is always the best possible outcome, beating even +2")
  (is (= (attack-deck/better :null :minus-2) :minus-2)
      "Null is always the worst possible outcome, losing even to -2")
  (testing "ties favor whichever was drawn first (the first argument)"
    (is (= (attack-deck/better :plus-1 :plus-1) :plus-1))
    (is (= (attack-deck/better :null :curse) :null))
    (is (= (attack-deck/better :curse :null) :curse))))

(deftest test-worse
  (is (= (attack-deck/worse :plus-1 :minus-1) :minus-1)
      "disadvantage keeps the numerically lower card")
  (is (= (attack-deck/worse :minus-1 :plus-1) :minus-1))
  (is (= (attack-deck/worse :minus-2 :null) :null)
      "Null is always the worst possible outcome, losing even to -2")
  (is (= (attack-deck/worse :times-2 :plus-2) :plus-2)
      "2x is always the best possible outcome, so it's never the one disadvantage keeps")
  (testing "ties favor whichever was drawn first (the first argument)"
    (is (= (attack-deck/worse :plus-0 :plus-0) :plus-0))
    (is (= (attack-deck/worse :times-2 :bless) :times-2))))
