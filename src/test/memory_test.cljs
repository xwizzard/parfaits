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

;; valid-turn-index/next-turn-index/winners moved to
;; ogres.app.turn-order (see turn_order_test.cljs) once Go Fish needed
;; the identical, already-generic logic.
