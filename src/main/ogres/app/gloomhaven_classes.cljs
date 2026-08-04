(ns ogres.app.gloomhaven-classes
  "The perk cards each Gloomhaven class can add to its attack modifier
   deck -- official class content, deliberately separate from
   ogres.app.attack-deck's generic kind/effect vocabulary.

   A class's perk card set is exactly the multiset of cards its perks can
   add: sum every card each perk contributes, weighted by how many times
   that perk may be taken, and the total equals the printed card count for
   all 17 classes. So this table is the perk sheet expressed as cards --
   the form the deck-edit model needs today, and the form a perk-based
   model would derive its deck from later.

   Cards are {:kind :count} plus, optionally, :rolling?, :effect, :amount
   and :target -- the same vocabulary attack-deck/card-face renders. An
   :element effect carries WHICH element in its amount rather than a
   quantity, and :target is the qualifier the printed cards write beneath
   the glyph: every heal and shield card in these decks is Self."
  (:require [ogres.app.attack-deck :as attack-deck]))

(def class-names
  "Display name per class code. The codes are not always the obvious
   abbreviation -- sk is Sunkeeper, ss Soothsinger, su Summoner."
  {:be "Berserker"
   :br "Brute"
   :bt "Beast Tyrant"
   :ch "Cragheart"
   :ds "Doomstalker"
   :el "Elementalist"
   :mt "Mindthief"
   :ns "Nightshroud"
   :ph "Plagueherald"
   :qm "Quartermaster"
   :sb "Sawbones"
   :sc "Scoundrel"
   :sk "Sunkeeper"
   :ss "Soothsinger"
   :su "Summoner"
   :sw "Spellweaver"
   :ti "Tinkerer"})

(def class-cards
  "Perk cards per class, keyed by class code."
  {
   ;; Berserker -- 15 cards
   :be
   [
    {:kind :plus-0 :count 4 :rolling? true :effect :wound}
    {:kind :plus-0 :count 2 :rolling? true :effect :heal :amount 1 :target :self}
    {:kind :plus-0 :count 2 :rolling? true :effect :stun}
    {:kind :plus-1 :count 2}
    {:kind :plus-2 :count 2 :effect :element :amount :fire}
    {:kind :plus-2 :count 2 :rolling? true}
    {:kind :plus-1 :count 1 :rolling? true :effect :disarm}
   ]

   ;; Brute -- 22 cards
   :br
   [
    {:kind :plus-0 :count 6 :rolling? true :effect :push :amount 1}
    {:kind :plus-1 :count 6}
    {:kind :plus-0 :count 2 :rolling? true :effect :add-target}
    {:kind :plus-0 :count 2 :rolling? true :effect :pierce :amount 3}
    {:kind :plus-0 :count 2 :rolling? true :effect :stun}
    {:kind :plus-0 :count 1 :rolling? true :effect :disarm}
    {:kind :plus-0 :count 1 :rolling? true :effect :muddle}
    {:kind :plus-1 :count 1 :effect :shield :amount 1 :target :self}
    {:kind :plus-3 :count 1}
   ]

   ;; Beast Tyrant -- 17 cards
   :bt
   [
    {:kind :plus-0 :count 6 :rolling? true :effect :heal :amount 1 :target :self}
    {:kind :plus-1 :count 3}
    {:kind :plus-0 :count 2 :rolling? true :effect :element :amount :earth}
    {:kind :plus-1 :count 2 :effect :immobilize}
    {:kind :plus-1 :count 2 :effect :wound}
    {:kind :plus-2 :count 2}
   ]

   ;; Cragheart -- 18 cards
   :ch
   [
    {:kind :plus-0 :count 4 :rolling? true :effect :element :amount :earth}
    {:kind :plus-1 :count 3}
    {:kind :plus-0 :count 2 :rolling? true :effect :element :amount :air}
    {:kind :plus-0 :count 2 :rolling? true :effect :push :amount 2}
    {:kind :plus-1 :count 2 :effect :immobilize}
    {:kind :plus-2 :count 2 :effect :muddle}
    {:kind :plus-2 :count 2}
    {:kind :minus-2 :count 1}
   ]

   ;; Doomstalker -- 17 cards
   :ds
   [
    {:kind :plus-1 :count 6}
    {:kind :plus-1 :count 4 :rolling? true}
    {:kind :plus-0 :count 2 :rolling? true :effect :add-target}
    {:kind :plus-0 :count 1 :effect :stun}
    {:kind :plus-1 :count 1 :effect :immobilize}
    {:kind :plus-1 :count 1 :effect :poison}
    {:kind :plus-1 :count 1 :effect :wound}
    {:kind :plus-2 :count 1 :effect :muddle}
   ]

   ;; Elementalist -- 24 cards
   :el
   [
    {:kind :plus-0 :count 4 :effect :element :amount :air}
    {:kind :plus-0 :count 4 :effect :element :amount :earth}
    {:kind :plus-0 :count 4 :effect :element :amount :fire}
    {:kind :plus-0 :count 4 :effect :element :amount :ice}
    {:kind :plus-1 :count 2 :effect :push :amount 1}
    {:kind :plus-2 :count 2}
    {:kind :plus-0 :count 1 :effect :add-target}
    {:kind :plus-0 :count 1 :effect :stun}
    {:kind :plus-1 :count 1 :effect :wound}
    {:kind :plus-1 :count 1}
   ]

   ;; Mindthief -- 20 cards
   :mt
   [
    {:kind :plus-0 :count 4 :rolling? true :effect :muddle}
    {:kind :plus-1 :count 4 :rolling? true}
    {:kind :plus-0 :count 3 :rolling? true :effect :pull :amount 1}
    {:kind :plus-0 :count 2 :rolling? true :effect :immobilize}
    {:kind :plus-2 :count 2 :effect :element :amount :ice}
    {:kind :plus-2 :count 2}
    {:kind :plus-0 :count 1}
    {:kind :plus-0 :count 1 :rolling? true :effect :disarm}
    {:kind :plus-0 :count 1 :rolling? true :effect :stun}
   ]

   ;; Nightshroud -- 19 cards
   :ns
   [
    {:kind :plus-0 :count 6 :rolling? true :effect :muddle}
    {:kind :minus-1 :count 2 :effect :element :amount :dark}
    {:kind :plus-0 :count 2 :rolling? true :effect :curse}
    {:kind :plus-0 :count 2 :rolling? true :effect :heal :amount 1 :target :self}
    {:kind :plus-1 :count 2 :effect :element :amount :dark}
    {:kind :plus-1 :count 2 :effect :invisible}
    {:kind :plus-1 :count 2}
    {:kind :plus-0 :count 1 :rolling? true :effect :add-target}
   ]

   ;; Plagueherald -- 20 cards
   :ph
   [
    {:kind :plus-1 :count 5}
    {:kind :plus-0 :count 3 :rolling? true :effect :poison}
    {:kind :plus-1 :count 3 :effect :element :amount :air}
    {:kind :plus-0 :count 2 :rolling? true :effect :curse}
    {:kind :plus-0 :count 2 :rolling? true :effect :immobilize}
    {:kind :plus-0 :count 2 :rolling? true :effect :stun}
    {:kind :plus-2 :count 2}
    {:kind :plus-0 :count 1}
   ]

   ;; Quartermaster -- 18 cards
   :qm
   [
    {:kind :plus-1 :count 4 :rolling? true}
    {:kind :plus-0 :count 3 :effect :refresh-item}
    {:kind :plus-0 :count 3 :rolling? true :effect :muddle}
    {:kind :plus-0 :count 2 :rolling? true :effect :pierce :amount 3}
    {:kind :plus-1 :count 2}
    {:kind :plus-2 :count 2}
    {:kind :plus-0 :count 1 :rolling? true :effect :add-target}
    {:kind :plus-0 :count 1 :rolling? true :effect :stun}
   ]

   ;; Sawbones -- 14 cards
   :sb
   [
    {:kind :plus-0 :count 4 :rolling? true :effect :wound}
    {:kind :plus-0 :count 2 :rolling? true :effect :heal :amount 3 :target :self}
    {:kind :plus-1 :count 2 :effect :immobilize}
    {:kind :plus-2 :count 2}
    {:kind :plus-2 :count 2 :rolling? true}
    {:kind :plus-0 :count 1 :effect :refresh-item}
    {:kind :plus-0 :count 1 :rolling? true :effect :stun}
   ]

   ;; Scoundrel -- 17 cards
   :sc
   [
    {:kind :plus-0 :count 4 :rolling? true :effect :poison}
    {:kind :plus-1 :count 4 :rolling? true}
    {:kind :plus-0 :count 2 :rolling? true :effect :muddle}
    {:kind :plus-0 :count 2 :rolling? true :effect :pierce :amount 3}
    {:kind :plus-2 :count 2}
    {:kind :plus-0 :count 1}
    {:kind :plus-0 :count 1 :rolling? true :effect :invisible}
    {:kind :plus-1 :count 1}
   ]

   ;; Sunkeeper -- 19 cards
   :sk
   [
    {:kind :plus-0 :count 4 :rolling? true :effect :element :amount :light}
    {:kind :plus-0 :count 4 :rolling? true :effect :heal :amount 1 :target :self}
    {:kind :plus-1 :count 4 :rolling? true}
    {:kind :plus-0 :count 2 :rolling? true :effect :shield :amount 1 :target :self}
    {:kind :plus-1 :count 2}
    {:kind :plus-0 :count 1}
    {:kind :plus-0 :count 1 :rolling? true :effect :stun}
    {:kind :plus-2 :count 1}
   ]

   ;; Soothsinger -- 16 cards
   :ss
   [
    {:kind :plus-0 :count 4 :rolling? true :effect :curse}
    {:kind :plus-1 :count 3 :rolling? true}
    {:kind :plus-4 :count 2}
    {:kind :plus-0 :count 1 :effect :stun}
    {:kind :plus-1 :count 1 :effect :disarm}
    {:kind :plus-1 :count 1 :effect :immobilize}
    {:kind :plus-2 :count 1 :effect :curse}
    {:kind :plus-2 :count 1 :effect :poison}
    {:kind :plus-2 :count 1 :effect :wound}
    {:kind :plus-3 :count 1 :effect :muddle}
   ]

   ;; Summoner -- 22 cards
   :su
   [
    {:kind :plus-0 :count 6 :rolling? true :effect :heal :amount 1 :target :self}
    {:kind :plus-1 :count 5}
    {:kind :plus-0 :count 2 :rolling? true :effect :poison}
    {:kind :plus-0 :count 2 :rolling? true :effect :wound}
    {:kind :plus-2 :count 2}
    {:kind :plus-0 :count 1}
    {:kind :plus-0 :count 1 :rolling? true :effect :element :amount :air}
    {:kind :plus-0 :count 1 :rolling? true :effect :element :amount :dark}
    {:kind :plus-0 :count 1 :rolling? true :effect :element :amount :earth}
    {:kind :plus-0 :count 1 :rolling? true :effect :element :amount :fire}
   ]

   ;; Spellweaver -- 18 cards
   :sw
   [
    {:kind :plus-1 :count 6}
    {:kind :plus-2 :count 2 :effect :element :amount :fire}
    {:kind :plus-2 :count 2 :effect :element :amount :ice}
    {:kind :plus-0 :count 1 :effect :stun}
    {:kind :plus-0 :count 1 :rolling? true :effect :element :amount :air}
    {:kind :plus-0 :count 1 :rolling? true :effect :element :amount :dark}
    {:kind :plus-0 :count 1 :rolling? true :effect :element :amount :earth}
    {:kind :plus-0 :count 1 :rolling? true :effect :element :amount :light}
    {:kind :plus-1 :count 1 :effect :curse}
    {:kind :plus-1 :count 1 :effect :immobilize}
    {:kind :plus-1 :count 1 :effect :wound}
   ]

   ;; Tinkerer -- 16 cards
   :ti
   [
    {:kind :plus-0 :count 3 :rolling? true :effect :muddle}
    {:kind :plus-0 :count 2 :rolling? true :effect :element :amount :fire}
    {:kind :plus-1 :count 2 :effect :heal :amount 2 :target :self}
    {:kind :plus-1 :count 2 :effect :immobilize}
    {:kind :plus-1 :count 2 :effect :wound}
    {:kind :plus-1 :count 2}
    {:kind :plus-0 :count 1 :effect :add-target}
    {:kind :plus-0 :count 1}
    {:kind :plus-3 :count 1}
   ]})

(defn cards-for
  "The perk cards `code`'s class may add, or nil for an unknown class."
  [code]
  (get class-cards code))

(defn card-count
  "How many perk cards `code`'s class has in total."
  [code]
  (reduce + 0 (map :count (cards-for code))))

(defn expressible?
  "True when every card in the table renders with the vocabulary
   attack-deck actually has -- no silent blanks. Guards the table against
   drifting away from the renderer."
  []
  (every?
   (fn [card]
     (let [{:keys [kind effect amount rolling? target]} card
           face (attack-deck/card-face kind {:effect effect :amount amount
                                             :rolling? rolling? :target target})]
       ;; A card must draw SOMETHING: its modifier, and -- when it carries
       ;; one -- its effect too. An effect always ends up in :effect-icon
       ;; now (it owns the medallion whenever there is one); this guards
       ;; against a card whose effect silently failed to resolve to a
       ;; glyph at all.
       (and (some? (or (:value face) (:glyph face)))
            (or (nil? effect) (some? (:effect-icon face))))))
   (mapcat val class-cards)))
