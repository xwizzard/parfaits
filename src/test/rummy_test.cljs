(ns rummy-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [ogres.app.rummy :as rummy]))

(deftest test-scoreable-set
  (testing "a fresh set of exactly 3 -- lays down all 3"
    (is (= (rummy/scoreable-set 3 0) 3)))
  (testing "a fresh set of all 4 held at once -- lays down all 4"
    (is (= (rummy/scoreable-set 4 0) 4)))
  (testing "only 2 held, nothing scored yet -- not enough for a fresh set"
    (is (= (rummy/scoreable-set 2 0) 0)))
  (testing "laying off the 4th -- exactly 3 already scored, 1 more in hand"
    (is (= (rummy/scoreable-set 1 3) 1)))
  (testing "3 already scored, but the player holds none of that rank -- nothing to lay off"
    (is (= (rummy/scoreable-set 0 3) 0)))
  (testing "a rank already fully scored (4) has nothing left to add"
    (is (= (rummy/scoreable-set 1 4) 0))
    (is (= (rummy/scoreable-set 4 4) 0)))
  (testing "1 or 2 already scored is not a reachable state for SET-melded
            cards (sets go down 3 or 4 at once), but defensively still
            refuses a stray 4th. Callers must derive the count with
            set-scored-count -- counting a run's cards here would make
            these two branches reachable and silently unplayable."
    (is (= (rummy/scoreable-set 1 1) 0))
    (is (= (rummy/scoreable-set 1 2) 0))))

(deftest test-set-scored-count-ignores-run-melds
  (testing "the shared :scored area holds set-melds and run-melds together;
            only set-melds count as progress toward a set"
    (let [set-card {:card/rank :five :card/suit :hearts}
          run-card {:card/rank :five :card/suit :spades :card/run-meld? true}]
      (is (= (rummy/set-scored-count []) 0))
      (is (= (rummy/set-scored-count [set-card set-card set-card]) 3))
      (is (= (rummy/set-scored-count [run-card]) 0)
          "a five that went down inside somebody's run is not set progress")
      (is (= (rummy/set-scored-count [run-card run-card run-card]) 0)
          "...and three of them still do not add up to a laid-down set")
      (is (= (rummy/set-scored-count [set-card run-card]) 1))))

  (testing "the combination that used to lock a legal set out of play:
            one five down in a run, a player holding three more"
    (let [run-five {:card/rank :five :card/suit :spades :card/run-meld? true}]
      (is (= (rummy/scoreable-set 3 (rummy/set-scored-count [run-five])) 3)
          "all three of the player's own fives lay down as a fresh set")))

  (testing "and the mirror: three fives arriving via runs must not let a
            fourth be laid off onto a set that was never laid down"
    (let [run-five (fn [suit] {:card/rank :five :card/suit suit :card/run-meld? true})
          scored (mapv run-five [:spades :hearts :clubs])]
      (is (= (rummy/scoreable-set 1 (rummy/set-scored-count scored)) 0)))))

(defn ^:private card [rank suit] {:card/rank rank :card/suit suit})

(deftest test-runs
  (testing "a clean run of exactly 3"
    (let [hand [(card :five :hearts) (card :six :hearts) (card :seven :hearts)]]
      (is (= (count (rummy/runs hand)) 1))
      (is (= (count (first (rummy/runs hand))) 3))))
  (testing "a 4-in-a-row is one run of length 4, not two overlapping runs of 3"
    (let [hand [(card :five :hearts) (card :six :hearts) (card :seven :hearts) (card :eight :hearts)]]
      (is (= (count (rummy/runs hand)) 1))
      (is (= (count (first (rummy/runs hand))) 4))))
  (testing "no run present -- only 2 consecutive, or none at all"
    (is (empty? (rummy/runs [(card :five :hearts) (card :six :hearts)])))
    (is (empty? (rummy/runs [(card :two :hearts) (card :seven :clubs) (card :king :spades)]))))
  (testing "a run mixed in with unrelated cards -- only the run itself returns"
    (let [hand [(card :two :clubs) (card :five :hearts) (card :six :hearts) (card :seven :hearts) (card :king :spades)]]
      (is (= (count (rummy/runs hand)) 1))
      (is (= (set (map :card/rank (first (rummy/runs hand)))) #{:five :six :seven}))))
  (testing "different suits with the same ranks don't combine into one run"
    (let [hand [(card :five :hearts) (card :six :clubs) (card :seven :hearts)]]
      (is (empty? (rummy/runs hand)))))
  (testing "ace is low only -- A,2,3 is a run; Q,K,A does NOT wrap"
    (let [ace-low [(card :ace :spades) (card :two :spades) (card :three :spades)]
          no-wrap [(card :queen :spades) (card :king :spades) (card :ace :spades)]]
      (is (= (count (rummy/runs ace-low)) 1))
      (is (empty? (rummy/runs no-wrap)))))
  (testing "an empty hand has no runs"
    (is (empty? (rummy/runs [])))))
