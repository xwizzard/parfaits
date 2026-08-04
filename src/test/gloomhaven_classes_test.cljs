(ns gloomhaven-classes-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [ogres.app.attack-deck :as attack-deck]
            [ogres.app.gloomhaven-classes :as classes]))

(def ^:private printed-counts
  "Cards physically printed for each class, counted from the card images
   independently of the perk sheets. The table is derived from the perks,
   so agreeing with this is a real cross-check rather than a restatement."
  {:be 15 :br 22 :bt 17 :ch 18 :ds 17 :el 24 :mt 20 :ns 19 :ph 20
   :qm 18 :sb 14 :sc 17 :sk 19 :ss 16 :su 22 :sw 18 :ti 16})

(deftest test-every-class-present
  (is (= (set (keys classes/class-cards)) (set (keys printed-counts))))
  (is (= (set (keys classes/class-names)) (set (keys printed-counts)))
      "every class has a display name")
  (is (= (count classes/class-cards) 17)))

(deftest test-card-counts-match-the-printed-decks
  (testing "a class's perk cards are exactly what its perks can add"
    (doseq [[code n] printed-counts]
      (is (= (classes/card-count code) n)
          (str (get classes/class-names code) " (" (name code) ")")))))

(deftest test-every-card-renders
  (testing "no card in the table falls through to a blank face"
    (is (classes/expressible?)))

  (testing "and the vocabulary it needs actually exists"
    (doseq [card (mapcat val classes/class-cards)
            :let [{:keys [kind effect]} card]]
      (is (contains? attack-deck/values kind) (str "unknown kind " kind))
      (is (or (nil? effect) (contains? attack-deck/effect-kinds effect))
          (str "unknown effect " effect)))))

(deftest test-brute
  (testing "spot-check one class against its printed sheet"
    (let [by (into {} (map (juxt (juxt :kind :effect :amount :rolling?) :count))
                   (classes/cards-for :br))]
      (is (= (get by [:plus-1 nil nil nil]) 6) "six plain +1")
      (is (= (get by [:plus-3 nil nil nil]) 1) "one +3")
      (is (= (get by [:plus-0 :push 1 true]) 6) "six rolling PUSH 1")
      (is (= (get by [:plus-0 :pierce 3 true]) 2) "two rolling PIERCE 3")
      (is (= (get by [:plus-0 :add-target nil true]) 2) "two rolling ADD TARGET")
      (is (= (get by [:plus-1 :shield 1 nil]) 1) "one +1 Shield 1"))))

(deftest test-rolling-and-element-faces
  (testing "rolling is carried onto the face"
    (is (:rolling? (attack-deck/card-face :plus-0 {:rolling? true})))
    (is (not (:rolling? (attack-deck/card-face :plus-0 {})))))

  (testing "an element's glyph comes from WHICH element, not a quantity"
    (is (= (:effect-icon (attack-deck/card-face :plus-1 {:effect :element :amount :fire}))
           "am-fire"))
    (is (= (:effect-icon (attack-deck/card-face :plus-0 {:effect :element :amount :dark}))
           "am-dark"))
    (is (= (attack-deck/effect-label :element :ice) "Element ice")
        "and reads as the element's name rather than a number"))

  (testing "the kinds the class decks introduced"
    (is (= (:value (attack-deck/card-face :plus-3)) "+3"))
    (is (= (:value (attack-deck/card-face :plus-4)) "+4"))
    (is (= (attack-deck/value :plus-4) 4))))

(deftest test-heal-and-shield-are-self-targeted
  (testing "every heal and shield card in these decks says Self"
    (let [cards (mapcat val classes/class-cards)
          healing (filter (comp #{:heal :shield} :effect) cards)]
      (is (seq healing))
      (is (every? (comp #{:self} :target) healing)
          "the printed cards all write Self beneath the glyph")
      (is (= (reduce + (map :count healing)) 27)
          "24 heal and 3 shield across the 17 classes")))

  (testing "and nothing else carries a target"
    (is (every? (fn [c] (or (nil? (:target c)) (#{:heal :shield} (:effect c))))
                (mapcat val classes/class-cards)))))
