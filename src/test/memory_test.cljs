(ns memory-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [ogres.app.memory :as memory]
            [ogres.app.vec :as vec]))

(deftest test-deal
  (let [cards (memory/deal)]
    (testing "a real standard deck -- 13 ranks in 4 suits, each exactly once"
      (is (= (count cards) 52))
      (is (= (count (into #{} cards)) 52) "no duplicate cards")
      (is (= (into #{} (map :card/suit) cards) (set memory/suits)))
      (is (= (into #{} (map :card/rank) cards) (set memory/ranks))))
    (testing "the deal carries no coordinates -- a card's INDEX is its slot,
              which is what lets the table move and scale as one unit"
      (is (every? (fn [c] (= (set (keys c)) #{:card/rank :card/suit})) cards)))))

(deftest test-pair?
  (testing "rank AND colour, which is what splits one deck into 26 pairs"
    (let [ks {:card/rank :king :card/suit :spades}
          kc {:card/rank :king :card/suit :clubs}
          kh {:card/rank :king :card/suit :hearts}
          kd {:card/rank :king :card/suit :diamonds}
          qs {:card/rank :queen :card/suit :spades}]
      (is (memory/pair? ks kc) "the two black kings")
      (is (memory/pair? kh kd) "the two red kings")
      (is (not (memory/pair? ks kh)) "same rank, different colour")
      (is (not (memory/pair? ks qs)) "same suit, different rank")
      (is (not (memory/pair? ks ks)) "a card cannot pair with itself")))

  (testing "every card in a deal has exactly one partner"
    (let [cards (memory/deal)]
      (is (every? (fn [c] (= 1 (count (filter #(memory/pair? c %) cards)))) cards))
      (is (= (count (filter (fn [[a b]] (memory/pair? a b))
                            (for [a cards b cards] [a b])))
             52)
          "52 ordered matches = 26 unordered pairs, the whole deck"))))

(deftest test-card-offset-lays-out-a-grid
  (testing "row-major, `columns` wide, one card pitch apart"
    (let [pitch-x (+ memory/card-width memory/card-gap)
          pitch-y (+ memory/card-height memory/card-gap)]
      (let [pad memory/table-padding]
        (is (= (memory/card-offset 0) (vec/Vec2. pad pad))
            "inset by the felt border -- the felt is part of the table")
        (is (= (memory/card-offset 1) (vec/Vec2. (+ pad pitch-x) pad)))
        (is (= (memory/card-offset (dec memory/columns))
               (vec/Vec2. (+ pad (* (dec memory/columns) pitch-x)) pad)))
        (is (= (memory/card-offset memory/columns) (vec/Vec2. pad (+ pad pitch-y)))
            "index `columns` wraps to the start of row 1"))))

  (testing "every card in a 52-card deal gets a distinct slot"
    (is (= (count (into #{} (map memory/card-offset) (range 52))) 52))))

(deftest test-table-size
  (testing "the unscaled footprint the table's bounding rect is built from"
    (let [[w h] (memory/table-size 52)]
      (is (= w (+ (* 2 memory/table-padding)
                  (- (* memory/columns (+ memory/card-width memory/card-gap))
                     memory/card-gap)))
          "8 columns wide plus felt on both sides")
      (is (= h (+ (* 2 memory/table-padding)
                  (- (* 4 (+ memory/card-height memory/card-gap)) memory/card-gap)))
          "52 cards at 13 wide is exactly 4 rows, no empty slots")))

  (testing "a part-empty last row still occupies a whole row"
    (is (= (memory/rows 52) 4) "a full deck fills 4 rows exactly")
    (is (= (memory/rows 13) 1))
    (is (= (memory/rows 14) 2)))

  (testing "an emptied table (every pair matched) stays a valid frame"
    (let [[w h] (memory/table-size 0)]
      (is (pos? w))
      (is (pos? h))))

  (testing "the felt is sized from the whole deck, not what is left on it"
    (is (= (memory/table-footprint) (memory/table-size memory/deck-size)))
    (is (= memory/deck-size 52))
    (is (= (memory/table-footprint) (memory/table-size 52))
        "a table placed empty is already full-deck sized, so what gets
         positioned and scaled is exactly what the deal will fill")
    (is (not= (memory/table-footprint) (memory/table-size 2))
        "which is the whole point -- sizing to the live card count would
         shrink the board under the players as pairs are matched")))

;; valid-turn-index/next-turn-index/winners moved to
;; ogres.app.turn-order (see turn_order_test.cljs) once Go Fish needed
;; the identical, already-generic logic.
