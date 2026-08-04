(ns attack-deck-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [ogres.app.attack-deck :as attack-deck]
            [ogres.app.game-type.games.gloomhaven :as gloomhaven]))

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

(deftest test-effect-label
  (testing "amount-taking effects show the number"
    (is (= (attack-deck/effect-label :push 2) "Push 2"))
    (is (= (attack-deck/effect-label :pierce 3) "Pierce 3"))
    (is (= (attack-deck/effect-label :shield 1) "Shield 1")))
  (testing "flag-only effects never show a number, even if one was given"
    (is (= (attack-deck/effect-label :stun nil) "Stun"))
    (is (= (attack-deck/effect-label :stun 5) "Stun"))
    (is (= (attack-deck/effect-label :disarm nil) "Disarm"))
    (is (= (attack-deck/effect-label :add-target nil) "Add Target")))
  (testing "no effect at all is nil, not an empty string"
    (is (nil? (attack-deck/effect-label nil nil)))
    (is (nil? (attack-deck/effect-label :not-a-real-effect 1)))))

(deftest test-card-face
  (testing "the modifier decides the fill, never the deck or the effect"
    (is (= (:fill (attack-deck/card-face :plus-1)) :positive))
    (is (= (:fill (attack-deck/card-face :minus-2)) :negative))
    (is (= (:fill (attack-deck/card-face :plus-0)) :neutral))
    (is (= (:value (attack-deck/card-face :minus-2)) "-2")))

  (testing "bless IS a 2x card and curse IS a null card"
    (let [b (attack-deck/card-face :bless)
          c (attack-deck/card-face :curse)]
      (is (= (:value b) "2x") "the medallion carries the modifier...")
      (is (= (:wings b) :bless) "...and the blessing rides in the wings")
      (is (= (:fill b) (:fill (attack-deck/card-face :times-2)))
          "so a bless card is coloured like the 2x card it is")
      (is (= (:glyph c) "am-null"))
      (is (= (:wings c) :curse))
      (is (= (:fill c) (:fill (attack-deck/card-face :null))))
      (is (nil? (:value c)) "curse shows the miss glyph, not a numeral")))

  (testing "only null and 2x carry the shuffle mark"
    (is (every? (comp :shuffle? attack-deck/card-face) [:null :times-2]))
    (is (not-any? (comp :shuffle? attack-deck/card-face)
                  [:minus-2 :minus-1 :plus-0 :plus-1 :plus-2 :bless :curse])
        "the one-shots are removed when drawn, not reshuffled"))

  (testing "an effect always owns the medallion, and the numeral rides along"
    (let [f (attack-deck/card-face :plus-1 {:effect :pierce})]
      (is (= (:effect-icon f) "am-pierce") "the effect is the medallion")
      (is (= (:value f) "+1") "and the modifier is still on the face, for the chip"))
    (is (nil? (:effect-icon (attack-deck/card-face :plus-1)))
        "no effect, no medallion glyph -- the numeral stays put")
    (is (nil? (:effect-icon (attack-deck/card-face :plus-1 {:effect :nonsense})))
        "an unknown effect is ignored rather than rendering a broken icon"))

  (testing "every effect the app models has a glyph to draw it with"
    (is (every? (fn [k] (some? (:effect-icon (attack-deck/card-face :plus-1 {:effect k}))))
                (keys attack-deck/effect-kinds))))

  (testing "Reduced Randomness flattens the four extremes, sign colour included"
    (let [red {:reduced? true}]
      (is (= (select-keys (attack-deck/card-face :times-2 red) [:fill :value])
             {:fill :positive :value "+2"}))
      (is (= (select-keys (attack-deck/card-face :null red) [:fill :value])
             {:fill :negative :value "-2"}))
      (is (nil? (:glyph (attack-deck/card-face :null red)))
          "the numeral replaces the miss glyph rather than sitting beside it")
      (is (:shuffle? (attack-deck/card-face :null red))
          "the variant is cosmetic -- it still triggers a reshuffle")
      (is (= (:wings (attack-deck/card-face :curse red)) :curse)
          "and a cursed card is still a one-shot")))

  (testing "an unrecognised kind degrades instead of rendering nothing"
    (is (some? (:value (attack-deck/card-face :not-a-kind))))))

(deftest test-effect-accents-match-the-token-badges
  (testing "a status looks the same on a card as it does on a token"
    ;; Both were sampled from the same reference art, and the card faces
    ;; were checked against the printed perk cards -- so if these ever
    ;; disagree, one of the two has drifted rather than both being right.
    (let [badges (->> (get-in gloomhaven/elements
                              [:gloomhaven/status-effects :token-badge :vocabulary])
                      (map (juxt :value :badge-color))
                      (into {}))
          shared (filter badges (keys attack-deck/effect-colors))]
      (is (seq shared) "the two vocabularies do overlap")
      (doseq [k shared]
        (is (= (:color (get attack-deck/effect-colors k)) (get badges k))
            (str (name k) " accent"))))))

(deftest test-every-effect-has-an-accent
  (testing "no effect renders its glyph on the bare fallback fill"
    (doseq [k (keys attack-deck/effect-kinds)
            :when (not= k :element)]
      (is (contains? attack-deck/effect-colors k) (str "no accent for " k))))

  (testing "every element has one too"
    (is (= (set (keys attack-deck/element-colors))
           #{:fire :ice :air :earth :light :dark})))

  (testing "an element's colour is the printed medallion's, measured"
    ;; Sampled from decks whose element blocks were read off a contact
    ;; sheet, not derived -- deriving them from the perk sheets is what put
    ;; a taupe on DARK, which is a deep navy on the card.
    (is (= (:color (get attack-deck/element-colors :dark)) "#163856"))
    (is (= (:color (get attack-deck/element-colors :ice)) "#34c2f0")))

  (testing "every element's field is its own orb at the common lightness"
    ;; The card is a darker wash of the element and the orb is the bright
    ;; thing on it, which is the two-tone arrangement the printed card has.
    (doseq [[k {:keys [field]}] attack-deck/element-colors]
      (is (re-find #"^oklch\(0\.47 " field) (str k " sits in the common band")))))

(deftest test-an-element-carries-its-orb-to-the-view
  (testing "the orb colour survives both layouts"
    ;; It is dropped from every other effect -- nothing is drawn behind
    ;; those glyphs -- so an element has to be exempted in both branches or
    ;; it silently loses the one colour that identifies it.
    (is (= (:element-color (attack-deck/card-face :plus-0 {:effect :element :amount :fire}))
           "#e45626")
        "on a card the element owns")
    (is (= (:element-color (attack-deck/card-face :plus-2 {:effect :element :amount :fire}))
           "#e45626")
        "and on one where it rides in the chit"))

  (testing "no other effect gets one"
    (is (nil? (:element-color (attack-deck/card-face :plus-0 {:effect :stun}))))
    (is (nil? (:element-color (attack-deck/card-face :plus-1 {:effect :muddle}))))))

(deftest test-card-face-carries-the-accent
  ;; Heal and Shield are the only effects that still paint an accent BEHIND
  ;; the glyph -- everything else recolours the whole card instead (see
  ;; test-an-effect-that-owns-the-card-owns-its-colour) and drops the accent
  ;; along with it, so there is nothing left to paint a seam with.
  (let [f (attack-deck/card-face :plus-0 {:effect :heal :amount 1})]
    (is (= (:color f) "#b02925")))
  (let [f (attack-deck/card-face :plus-1 {:effect :stun})]
    (is (nil? (:color f)))
    (is (= (:field-color f) "oklch(0.47 0.067 258.5)")
        "the colour moved to the card instead")))

(deftest test-an-effect-that-owns-the-card-owns-its-colour
  (testing "an effect recolours the whole card and drops the diamond's own paint"
    ;; The printed cards do this: a rolling stun is a blue card, not a
    ;; brown card wearing a blue diamond -- and it no longer matters what
    ;; kind the card itself is, now that the effect owns the medallion
    ;; whether the modifier is +0 or +2.
    (doseq [kind [:plus-0 :plus-1 :plus-2]]
      (let [f (attack-deck/card-face kind {:effect :stun :rolling? true})]
        (is (= (:field-color f) "oklch(0.47 0.067 258.5)") (str kind))
        (is (:plain-medallion? f) (str kind))
        (is (nil? (:color f)) (str kind " -- no accent left to draw behind the glyph")))))

  (testing "it does so whether or not the card rolls"
    ;; A printed +0 stun that does NOT roll is just as blue -- the colour
    ;; follows the effect, not the rolling flag.
    (is (= (:field-color (attack-deck/card-face :plus-0 {:effect :stun}))
           (:field-color (attack-deck/card-face :plus-0 {:effect :stun :rolling? true})))))

  (testing "elements colour the card by which element"
    (let [f (attack-deck/card-face :plus-0 {:effect :element :amount :ice})]
      (is (= (:field-color f) "oklch(0.47 0.130 225.1)"))
      (is (:plain-medallion? f))))

  (testing "heal sits on a fixed neutral instead, whatever its kind"
    ;; Measured on the printed Frosthaven cards: a rolling +0 heal and a
    ;; +1 heal sit on the SAME parchment brown, not on the modifier's own
    ;; sign -- so the value is one shared constant, not derived per kind.
    (doseq [kind [:plus-0 :plus-1 :plus-2]]
      (let [f (attack-deck/card-face kind {:effect :heal :amount 1 :target :self})]
        (is (= (:field-color f) attack-deck/neutral-field)
            (str kind " heal sits on the fixed neutral"))
        (is (nil? (:plain-medallion? f)) (str kind " heal keeps its diamond"))
        (is (some? (:color f)) (str kind " heal keeps its own accent")))))

  (testing "shield recolours the card like every other effect, unlike heal"
    ;; The printed shield card has no second tone: its accent IS the field,
    ;; measured -- there is nothing to distinguish it from a plain diamond
    ;; painted the same colour the card already is.
    (doseq [kind [:plus-0 :plus-1 :plus-2]]
      (let [f (attack-deck/card-face kind {:effect :shield :amount 1 :target :self})]
        (is (= (:field-color f) "oklch(0.47 0.057 53.8)") (str kind " shield"))
        (is (:plain-medallion? f) (str kind " shield goes plain"))
        (is (nil? (:color f)) (str kind " shield has no separate accent left to draw")))))

  (testing "the modifier, whatever its kind, no longer drives the field at all"
    ;; A +2 muddle and a +0 rolling muddle recolour the card identically --
    ;; the OLD model kept a numbered card on its own sign with the effect's
    ;; colour moved to a disc; that disc is gone along with the badge it
    ;; served, because the modifier no longer competes for the medallion.
    (is (= (:field-color (attack-deck/card-face :plus-2 {:effect :muddle}))
           (:field-color (attack-deck/card-face :plus-0 {:effect :muddle}))))
    (is (= (:fill (attack-deck/card-face :plus-2 {:effect :muddle})) :positive)
        "the sign itself is unchanged -- it just no longer drives the field"))

  (testing "the three effects that land on someone say WHO, whatever the kind"
    ;; Their glyph is already a drop, a shield or an arm, so the name adds
    ;; nothing the picture does not carry; who it lands on is the open
    ;; question, and now that they always occupy the medallion this is the
    ;; same caption a +0 card would show.
    (doseq [e [:heal :shield :strengthen]
            kind [:plus-0 :plus-1 :plus-2]]
      (is (= (:caption (attack-deck/card-face kind {:effect e :amount 1 :target :self}))
             "Self")
          (str kind " " (name e) " says its target")))
    (is (= (:caption (attack-deck/card-face :plus-1 {:effect :heal :amount 1 :target :ally}))
           "Ally"))
    (is (nil? (:amount-label (attack-deck/card-face :plus-1 {:effect :strengthen})))
        "strengthen has no quantity to put anywhere"))

  (testing "the raw palette key never leaks into a face"
    ;; :field is the palette's word for it; a face says :field-color or
    ;; says nothing. A leak would put an unused colour on every card map.
    (doseq [kind [:plus-0 :plus-1 :minus-1]
            effect (keys attack-deck/effect-kinds)]
      (is (nil? (:field (attack-deck/card-face kind {:effect effect :amount 1})))
          (str kind " " effect)))))

(deftest test-every-effect-that-takes-the-card-has-a-field
  (testing "no effect can own a card without a colour to paint it"
    (doseq [k (keys attack-deck/effect-kinds)
            :when (not (contains? #{:element :heal} k))]
      (is (some? (:field (get attack-deck/effect-colors k)))
          (str "no field colour for " k))))

  (testing "every element has one too"
    (doseq [k (keys attack-deck/element-colors)]
      (is (some? (:field (get attack-deck/element-colors k))) (str k))))

  (testing "heal alone has none, which is what keeps it neutral"
    ;; Shield looks like it belongs here too -- its accent is a muted
    ;; brown -- but the printed shield card has no second tone to keep: see
    ;; effect-colors' own docstring.
    (is (nil? (:field (get attack-deck/effect-colors :heal))))
    (is (some? (:field (get attack-deck/effect-colors :shield))))))

(deftest test-a-plus-0-effect-card-still-shows-its-plus-0
  ;; The printed Frosthaven cards write "+0" on the modifier's own chip
  ;; regardless of value -- unlike Gloomhaven's now-superseded arrangement,
  ;; where a +0 WAS the medallion and a redundant zero there was worth
  ;; dropping. Once the modifier is always a small chip, showing it costs
  ;; nothing, so it is never omitted.
  (let [f (attack-deck/card-face :plus-0 {:effect :push :amount 1 :rolling? true})]
    (is (= (:value f) "+0") "the modifier is still on the face, for the chip")
    (is (nil? (:glyph f)))
    (is (= (:effect-icon f) "am-push") "and the effect owns the medallion")
    (is (= (:amount f) 1)))

  (testing "a +1 carrying an effect shows its +1 the same way"
    (let [f (attack-deck/card-face :plus-1 {:effect :shield :amount 1})]
      (is (= (:value f) "+1"))
      (is (= (:amount f) 1) "and the shield's own value alongside it")))

  (testing "a plain +0 with no effect is still a +0 card"
    (is (= (:value (attack-deck/card-face :plus-0)) "+0"))))

(deftest test-effect-quantities
  (testing "quantities travel with the effect"
    (is (= (:amount (attack-deck/card-face :plus-0 {:effect :pierce :amount 3})) 3))
    (is (= (:amount (attack-deck/card-face :plus-1 {:effect :heal :amount 1})) 1)))

  (testing "an element's amount is which element, never a quantity"
    (let [f (attack-deck/card-face :plus-1 {:effect :element :amount :fire})]
      (is (nil? (:amount f)) "so it cannot render as a number")
      (is (= (:effect-icon f) "am-fire"))))

  (testing "effects that take no amount carry none"
    (is (nil? (:amount (attack-deck/card-face :plus-0 {:effect :stun}))))))

(deftest test-amount-labels
  (testing "effects that ADD read signed; ones that reduce do not"
    ;; Heal is the last effect whose number is still drawn on its own --
    ;; inside the drop -- so it is where the sign still has to be right.
    (is (= (:amount-label (attack-deck/card-face :plus-0 {:effect :heal :amount 1})) "+1"))
    (is (= (:amount-label (attack-deck/card-face :plus-0 {:effect :shield :amount 1})) "1")
        "a shield is a value, not an increment")
    (is (= (:caption (attack-deck/card-face :plus-0 {:effect :pierce :amount 3})) "Pierce 3")
        "pierce reduces the target's shield, so a + would misread"))

  (testing "a quantity that reads as a phrase takes the caption slot instead"
    ;; A lone 1 under an arrow could be a distance, a number of targets, or
    ;; damage. The word says which, so the bare number would only repeat it.
    (doseq [[e want] [[:push "Push 1"] [:pull "Pull 1"] [:pierce "Pierce 1"]
                      [:add-target "+1 Target"]]]
      (let [f (attack-deck/card-face :plus-0 {:effect e :amount 1})]
        (is (= (:caption f) want) (str (name e) " reads as a phrase"))
        (is (nil? (:amount-label f)) (str (name e) " does not also say it twice"))
        (is (= (:amount f) 1) "the quantity itself is still on the face"))))

  (testing "no amount, no label"
    (is (nil? (:amount-label (attack-deck/card-face :plus-0 {:effect :stun}))))
    (is (nil? (:amount-label (attack-deck/card-face :plus-1 {:effect :element :amount :fire})))))

  (testing "heal uses the solid drop, so a number can sit over it"
    (is (= (:effect-icon (attack-deck/card-face :plus-0 {:effect :heal :amount 1}))
           "am-heal-solid"))))

(deftest test-effect-targets
  (testing "the qualifier the printed cards write beneath the glyph"
    (let [f (attack-deck/card-face :plus-0 {:effect :heal :amount 1 :target :self})]
      (is (= (:target f) :self))
      (is (= (:caption f) "Self")))
    (is (= (:caption (attack-deck/card-face :plus-0 {:effect :shield :amount 1 :target :ally}))
           "Ally"))
    (is (= (:caption (attack-deck/card-face :plus-1 {:effect :shield :amount 1 :target :ally}))
           "Ally")
        "and a numbered card writes it the same way, now that it shares the medallion"))

  (testing "no target, no caption"
    (is (nil? (:caption (attack-deck/card-face :plus-0 {:effect :heal :amount 1}))))
    (is (nil? (:caption (attack-deck/card-face :plus-1)))
        "and a card with no effect cannot carry one either"))

  (testing "a target needs an effect to qualify"
    (is (nil? (:target (attack-deck/card-face :plus-1 {:target :self}))))))

(deftest test-element-cards-say-what-they-do
  (testing "an element glyph is a noun, so the card names the verb"
    ;; Without it the card shows a flame and leaves the reader to guess
    ;; whether it consumes fire, requires it, or makes it.
    (doseq [e [:fire :ice :air :earth :light :dark]]
      (is (= (:caption (attack-deck/card-face :plus-0 {:effect :element :amount e}))
             "Create")
          (str (name e) " card"))))

  (testing "every element says it, in one wording"
    (is (= 1 (count (into #{}
                          (map #(:caption (attack-deck/card-face :plus-1 {:effect :element :amount %})))
                          (keys attack-deck/element-colors))))))

  (testing "a stated target still wins the slot"
    ;; One caption slot, and a named target is the more specific fact.
    (is (= (:caption (attack-deck/card-face :plus-0 {:effect :element :amount :fire :target :self}))
           "Self")))

  (testing "an effect that keeps its number inside the glyph leaves the slot"
    ;; Heal's number sits in the drop, so the word is free for its target.
    (is (= (:caption (attack-deck/card-face :plus-0 {:effect :heal :amount 1 :target :self}))
           "Self"))))

(deftest test-conditions-name-themselves
  (testing "an effect with no quantity spends the slot on its own name"
    ;; Poison and wound are both a dark mark on a pale field at panel
    ;; size; the word is what tells them apart without learning a glyph.
    (is (= (:caption (attack-deck/card-face :plus-0 {:effect :stun})) "Stun"))
    (is (= (:caption (attack-deck/card-face :plus-0 {:effect :poison})) "Poison"))
    (is (= (:caption (attack-deck/card-face :plus-1 {:effect :invisible})) "Invisible")
        "and a numbered card names it the same way, now that it shares the medallion"))

  (testing "every quantity-free effect has one, and it is that effect's label"
    (doseq [[k {:keys [label amount?]}] attack-deck/effect-kinds
            :when (and (not amount?) (not= k :element))]
      (is (= (:caption (attack-deck/card-face :plus-0 {:effect k})) label)
          (str (name k) " names itself"))))

  (testing "effects whose quantity stands alone give the slot to the number"
    ;; Push, Pull and Add Target read as a phrase instead -- see
    ;; test-amount-labels -- so their quantity IS the caption.
    (doseq [[k {:keys [amount?]}] attack-deck/effect-kinds
            :when (and amount?
                       (not (contains? #{:element :push :pull :pierce :add-target} k)))]
      (is (nil? (:caption (attack-deck/card-face :plus-0 {:effect k :amount 1})))
          (str (name k) " has a number to show there"))))

  (testing "a stated target still wins the slot from a name"
    (is (= (:caption (attack-deck/card-face :plus-0 {:effect :invisible :target :self}))
           "Self"))))

(deftest test-custom-effect-card
  ;; No Gloomhaven class carries one of these -- every printed instance
  ;; found while checking this was Frosthaven's -- so nothing here is
  ;; exercised by the class-deck table today. It exists for whichever
  ;; source eventually supplies that data.
  (testing "a custom effect is prose, not a symbol"
    (let [f (attack-deck/card-face :plus-1 {:effect :custom :text "Add +2 for each ally adjacent to the target."})]
      (is (= (:custom-text f) "Add +2 for each ally adjacent to the target."))
      (is (nil? (:effect-icon f)) "there is no glyph to reach for")
      (is (nil? (:caption f)) "or a caption slot underneath one")
      (is (nil? (:amount f)))))

  (testing "the field is the same fixed neutral Heal sits on"
    ;; Measured on the printed cards (Coral, Banner Spear, Boneshaper):
    ;; the field is the ordinary brown parchment a plain +0 is printed on,
    ;; not a colour derived from what the custom effect happens to do.
    (is (= (:field-color (attack-deck/card-face :plus-1 {:effect :custom :text "x"}))
           attack-deck/neutral-field))
    (is (nil? (:plain-medallion? (attack-deck/card-face :plus-1 {:effect :custom :text "x"})))
        "there is no diamond to strip the paint from in the first place"))

  (testing "the modifier still shows, whatever its kind"
    ;; The printed custom cards keep the ordinary chip -- a custom effect
    ;; explaining itself in prose does not also excuse the modifier from
    ;; being shown.
    (doseq [kind [:plus-0 :plus-1 :plus-2]]
      (is (= (:value (attack-deck/card-face kind {:effect :custom :text "x"}))
             (:value (attack-deck/card-face kind)))
          (str kind " keeps its own numeral"))))

  (testing "rolling still carries onto a custom card"
    (is (:rolling? (attack-deck/card-face :plus-0 {:effect :custom :text "x" :rolling? true})))
    (is (not (:rolling? (attack-deck/card-face :plus-0 {:effect :custom :text "x"})))))

  (testing "custom is deliberately outside the fixed vocabulary"
    ;; It has no :label/:amount?, unlike every effect the composition
    ;; editor can actually offer -- a custom effect is never something a
    ;; GM assembles from the generic push/pull/condition parts, only
    ;; something a data source supplies pre-written.
    (is (not (contains? attack-deck/effect-kinds :custom)))))

(deftest test-custom-effect-value-chip
  ;; gloomhavensecretariat's own custom-effect text embeds the one token
  ;; this app's renderer understands as a shape rather than a word --
  ;; %game.attackmodifier.plusN%/minusN% -- the same way a printed custom
  ;; card embeds a small coloured circle mid-sentence.
  (testing "plain prose with no token is a single unsplit segment"
    (let [f (attack-deck/card-face :plus-1 {:effect :custom :text "Add +2 for each ally adjacent to the target."})]
      (is (= (:custom-text f) "Add +2 for each ally adjacent to the target.")
          "flattened text is untouched when there is nothing to flatten")))

  (testing "a token mid-sentence becomes a chip, and the plain text keeps its number"
    (let [f (attack-deck/card-face :plus-1 {:effect :custom :text "If you performed a tides action this round, %game.attackmodifier.plus2% instead."})]
      (is (= (:custom-text f) "If you performed a tides action this round, +2 instead.")
          "the flattened text reads naturally, chip standing in for its own number")
      (is (= (:custom-segments f)
             ["If you performed a tides action this round, "
              {:chip "+2" :sign :positive}
              " instead."])
          "the view gets the same prose as a vector of strings and chip maps")))

  (testing "minus tokens read negative"
    (let [f (attack-deck/card-face :plus-1 {:effect :custom :text "%game.attackmodifier.minus1%"})]
      (is (= (:custom-segments f) [{:chip "-1" :sign :negative}])
          "a token that opens/closes the sentence leaves no empty string either side")
      (is (= (:custom-text f) "-1"))))

  (testing "more than one token in the same sentence, each its own chip"
    (let [f (attack-deck/card-face :plus-1 {:effect :custom :text "%game.attackmodifier.plus1% now, %game.attackmodifier.minus1% later."})]
      (is (= (:custom-segments f)
             [{:chip "+1" :sign :positive} " now, " {:chip "-1" :sign :negative} " later."]))
      (is (= (:custom-text f) "+1 now, -1 later."))))

  (testing "nil text carries through as nil, not an empty vector"
    (is (nil? (:custom-segments (attack-deck/card-face :plus-1 {:effect :custom}))))
    (is (nil? (:custom-text (attack-deck/card-face :plus-1 {:effect :custom}))))))
