(ns memory-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [ogres.app.memory :as memory]
            [ogres.app.vec :as vec]))

(deftest test-deal
  (let [cards (memory/deal memory/default-difficulty)]
    (testing "two copies of every card in play, across all four suits"
      (is (= (count cards) 48))
      (is (= (count (into #{} cards)) 24) "24 distinct faces")
      (is (every? (fn [[_ n]] (= n memory/copies)) (frequencies cards))
          "each face dealt exactly twice, so every card has an identical
           partner and nothing is left over")
      (is (= (into #{} (map :card/suit) cards) (set memory/suits)))
      (is (= (set memory/suits) #{:clubs :diamonds :hearts :spades})
          "all four suits are safe to deal once a match means two
           IDENTICAL cards rather than two same-coloured ones")
      (is (= (into #{} (map :card/rank) cards)
             (set (memory/ranks-for-difficulty memory/default-difficulty)))))
    (testing "the deal carries no coordinates -- a card's INDEX is its slot,
              which is what lets the table move and scale as one unit"
      (is (every? (fn [c] (= (set (keys c)) #{:card/rank :card/suit})) cards)))))

(deftest test-pair?
  (testing "identical cards, which is what a match looks like at the table"
    (let [ks {:card/rank :king :card/suit :spades}
          kh {:card/rank :king :card/suit :hearts}
          qs {:card/rank :queen :card/suit :spades}]
      (is (memory/pair? ks ks) "the two copies of the king of spades")
      (is (not (memory/pair? ks kh))
          "same rank and both kings, but visibly different cards -- this
           is the case that used to match and read as a bug")
      (is (not (memory/pair? ks qs)) "same suit, different rank")))

  (testing "every card in a deal has exactly one partner"
    (let [cards (memory/deal memory/max-difficulty)]
      ;; Compared by INDEX, since two partners are equal as values --
      ;; being identical is the whole point of the new rule.
      (is (every? (fn [i]
                    (= 1 (count (for [j (range (count cards))
                                      :when (and (not= i j)
                                                 (memory/pair? (nth cards i) (nth cards j)))]
                                  j))))
                  (range (count cards))))
      (is (= 52 (count (into #{} cards)))
          "52 unordered pairs at the largest size, the whole double deck"))))

(deftest test-difficulty-scaling
  (testing "the two ends the sizes run between"
    (is (= (memory/deck-size 0) 32)
        "smallest: aces and court cards only, four ranks in four suits,
         two copies each")
    (is (= (memory/ranks-for-difficulty 0) memory/face-ranks))
    (is (= (memory/deck-size memory/max-difficulty) 104)
        "largest: every rank, both copies, all four suits")
    (is (= (set (memory/ranks-for-difficulty memory/max-difficulty))
           (set memory/ranks))
        "the hardest game holds every rank there is"))

  (testing "one numbered rank per step -- 4 suits x 2 copies = 8 cards"
    (is (every? (fn [level] (= 8 (- (memory/deck-size (inc level))
                                    (memory/deck-size level))))
                (range memory/max-difficulty)))
    (is (= (map memory/deck-size (range 0 (inc memory/max-difficulty)))
           [32 40 48 56 64 72 80 88 96 104])))

  (testing "numbered ranks come in counting up from the two"
    (is (= (memory/ranks-for-difficulty 1) [:ace :jack :queen :king :two]))
    (is (= (memory/ranks-for-difficulty 3)
           [:ace :jack :queen :king :two :three :four])))

  (testing "out-of-range levels clamp rather than dealing nonsense"
    (is (= (memory/deck-size -5) (memory/deck-size 0)))
    (is (= (memory/deck-size 99) (memory/deck-size memory/max-difficulty)))
    (is (= (memory/difficulty {}) memory/default-difficulty)
        "a table with no size stored reads as the default"))

  (testing "a harder game grows the table DOWNWARDS only"
    (let [[w0 h0] (memory/table-footprint {:memory/difficulty 0})
          [w9 h9] (memory/table-footprint {:memory/difficulty memory/max-difficulty})]
      (is (= w0 w9) "width is fixed -- the grid is always `columns` wide")
      (is (> h9 h0))
      (is (= (memory/rows (memory/deck-size memory/max-difficulty)) 8)
          "the largest game is 8 rows...")
      (is (= (memory/rows (memory/deck-size memory/default-difficulty)) 4)
          "...double the 4 rows the default sits at")))

  (testing "the default keeps the table at its familiar footprint"
    (is (= (memory/deck-size memory/default-difficulty) 48))
    (is (= (memory/table-footprint {}) (memory/table-footprint {:memory/difficulty 2})))))

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

  (testing "the felt is sized from the chosen size, not what is left on it"
    (let [table {:memory/difficulty 4}]
      (is (= (memory/table-footprint table)
             (memory/table-size (memory/deck-size 4)))
          "a table placed empty is already sized for its whole deal, so
           what gets positioned and scaled is exactly what will fill it")
      (is (not= (memory/table-footprint table) (memory/table-size 2))
          "which is the whole point -- sizing to the live card count
           would shrink the board under the players as pairs are
           matched"))))

;; valid-turn-index/next-turn-index/winners moved to
;; ogres.app.turn-order (see turn_order_test.cljs) once Go Fish needed
;; the identical, already-generic logic.
