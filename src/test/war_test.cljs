(ns war-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [ogres.app.war :as war]))

(deftest test-tied-for-highest
  (testing "a clear winner"
    (is (= (war/tied-for-highest {1 {:card/rank :king} 2 {:card/rank :seven} 3 {:card/rank :two}}) #{1})))
  (testing "a 2-way tie"
    (is (= (war/tied-for-highest {1 {:card/rank :queen} 2 {:card/rank :queen} 3 {:card/rank :five}}) #{1 2})))
  (testing "a 3+ way tie -- player 4's :two isn't part of it"
    (is (= (war/tied-for-highest {1 {:card/rank :ten} 2 {:card/rank :ten} 3 {:card/rank :ten} 4 {:card/rank :two}})
           #{1 2 3})))
  (testing "ace beats a king -- ace-high, the standard War convention"
    (is (= (war/tied-for-highest {1 {:card/rank :ace} 2 {:card/rank :king}}) #{1})))
  (testing "a single play is trivially the winner"
    (is (= (war/tied-for-highest {1 {:card/rank :two}}) #{1}))))
