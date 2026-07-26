(ns crazy-eights-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [ogres.app.crazy-eights :as crazy-eights]))

(deftest test-playable?
  (testing "an 8 is always playable, regardless of the top card"
    (is (true? (crazy-eights/playable? {:card/rank :eight} {:card/rank :two :card/suit :clubs} nil)))
    (is (true? (crazy-eights/playable? {:card/rank :eight} {:card/rank :king :card/suit :hearts} :spades))))
  (testing "matching by rank, top card not an 8"
    (is (true? (crazy-eights/playable? {:card/rank :five :card/suit :clubs}
                                        {:card/rank :five :card/suit :hearts} nil))))
  (testing "matching by suit, top card not an 8"
    (is (true? (crazy-eights/playable? {:card/rank :two :card/suit :hearts}
                                        {:card/rank :five :card/suit :hearts} nil))))
  (testing "neither rank nor suit matches, top card not an 8 -- illegal"
    (is (false? (crazy-eights/playable? {:card/rank :two :card/suit :clubs}
                                         {:card/rank :five :card/suit :hearts} nil))))
  (testing "top card IS an 8 -- only the declared suit (or another 8) is legal, its own
            printed rank/suit is irrelevant to matching"
    (is (true? (crazy-eights/playable? {:card/rank :two :card/suit :spades}
                                        {:card/rank :eight} :spades))
        "matches the declared suit")
    (is (false? (crazy-eights/playable? {:card/rank :two :card/suit :hearts}
                                         {:card/rank :eight} :spades))
        "wrong suit, and rank :two can't match a suit-less top card by rank either")
    (is (false? (crazy-eights/playable? {:card/rank :nine :card/suit :clubs}
                                         {:card/rank :eight} :spades))
        "a different rank entirely doesn't accidentally match just because the top
         card also happens to be rank :eight -- rank-matching is suppressed outright
         once the top card is an 8, not compared against :eight")))

(deftest test-playable-cards
  (let [top {:card/rank :five :card/suit :hearts}
        hand [{:db/id 1 :card/rank :five :card/suit :clubs}
              {:db/id 2 :card/rank :two :card/suit :hearts}
              {:db/id 3 :card/rank :two :card/suit :clubs}
              {:db/id 4 :card/rank :eight}]]
    (is (= (set (map :db/id (crazy-eights/playable-cards hand top nil))) #{1 2 4})
        "rank match (1), suit match (2), and the wild 8 (4) -- the lone
         no-match card (3) is excluded"))
  (is (empty? (crazy-eights/playable-cards [] {:card/rank :five :card/suit :hearts} nil))
      "an empty hand has nothing playable"))
