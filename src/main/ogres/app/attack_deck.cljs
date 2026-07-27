(ns ogres.app.attack-deck
  "Pure attack-modifier-deck logic for the Gloomhaven family of games
   ('x-haven' -- Gloomhaven, Frosthaven, Jaws of the Lion all share this
   mechanic nearly verbatim) -- no DataScript, no UI; mirrors the role
   ogres.app.cards/ogres.app.dice play for their own systems. See
   events.cljs's :attack-deck/* methods and component/panel_attack_deck.cljs.

   Deliberately named around the mechanic, not any single game -- a
   future Frosthaven module can reuse this exact namespace the same way
   multiple card games already share ogres.app.cards.")

(def standard-composition
  "kind -> count for a freshly-created standard 20-card attack modifier
   deck, per the Gloomhaven rulebook (p.5/p.10): 6x +0, 5x +1, 5x -1,
   1x +2, 1x -2, 1x Null (the attack does 0 damage), 1x 2x (the attack
   is doubled). BLESS/CURSE are never part of this base composition --
   they're added to a deck later, one at a time (see events.cljs's
   :attack-deck/add-bless/add-curse)."
  {:minus-2 1
   :minus-1 5
   :plus-0  6
   :plus-1  5
   :plus-2  1
   :null    1
   :times-2 1})

(def ^:private worst-value
  "Sentinel comparison value for :null/:curse -- always worse than any
   numbered card, regardless of how negative."
  -1000)

(def ^:private best-value
  "Sentinel comparison value for :times-2/:bless -- always better than
   any numbered card, regardless of how positive."
  1000)

(def ^:private values
  {:minus-2 -2 :minus-1 -1 :plus-0 0 :plus-1 1 :plus-2 2
   :null worst-value :curse worst-value
   :times-2 best-value :bless best-value})

(defn value
  "A comparable number for `kind`, used by `better`/`worse` and for
   display -- :null/:curse resolve to a sentinel worst value and
   :times-2/:bless to a sentinel best one, since neither is a plain
   additive modifier (Null means the attack does 0 damage outright, 2x
   doubles the pre-modifier attack value; both are always the best or
   worst possible outcome of a draw, not a number on the same scale as
   -2..+2)."
  [kind]
  (get values kind))

(defn shuffle-triggering?
  "True for the two standard-deck kinds carrying the 'shuffle' icon
   (p.11) -- when either is drawn, the deck's discard pile is reshuffled
   back into the draw pile at the end of the round (see events.cljs's
   :attack-deck/draw, which sets :deck/needs-reshuffle?, and
   :attack-deck/reshuffle-flagged, the manual action that acts on it)."
  [kind]
  (contains? #{:null :times-2} kind))

(defn removed-on-draw?
  "True for BLESS/CURSE -- one-shot cards that are removed from the deck
   entirely when drawn, rather than moving to the discard pile like
   every other kind (p.11: 'it should be removed from the player's deck
   instead of being placed into the discard')."
  [kind]
  (contains? #{:bless :curse} kind))

(defn better
  "Which of two drawn kinds Advantage keeps -- the numerically higher
   `value`, ties favoring `kind-a` (whichever was drawn first; p.20:
   'If there is ambiguity about which card drawn is better or worse, use
   whichever card was drawn first')."
  [kind-a kind-b]
  (if (> (value kind-b) (value kind-a)) kind-b kind-a))

(defn worse
  "Which of two drawn kinds Disadvantage keeps -- the numerically lower
   `value`, ties favoring `kind-a`, same tie-break rule as `better`."
  [kind-a kind-b]
  (if (< (value kind-b) (value kind-a)) kind-b kind-a))

(def effect-kinds
  "Attack-modifier-card attached effects (p.10-11's 'special effects of
   the attack') -- a small, fixed vocabulary covering what real class
   perks actually add (confirmed against a real class's perk list via
   gloomhavensecretariat's brute.json during the follow-up research
   pass): push/pull/pierce/shield/heal take a numeric amount, the rest
   are a plain flag. Deliberately distinct from the token :token-badge
   status-effect vocabulary (game_type/games/gloomhaven.cljs) -- this
   describes what ONE drawn card does for ONE attack, not an ongoing
   per-token condition, even though several names overlap conceptually
   (stun/disarm/muddle/immobilize/poison/wound). Only ever attached to
   one of the 7 plain standard-composition kinds -- BLESS/CURSE stay
   effect-free, already their own one-shot mechanic (see add-bless/
   add-curse)."
  {:push        {:label "Push"        :amount? true}
   :pull        {:label "Pull"        :amount? true}
   :pierce      {:label "Pierce"      :amount? true}
   :shield      {:label "Shield"      :amount? true}
   :heal        {:label "Heal"        :amount? true}
   :stun        {:label "Stun"        :amount? false}
   :disarm      {:label "Disarm"      :amount? false}
   :muddle      {:label "Muddle"      :amount? false}
   :immobilize  {:label "Immobilize"  :amount? false}
   :poison      {:label "Poison"      :amount? false}
   :wound       {:label "Wound"       :amount? false}
   :invisible   {:label "Invisible"   :amount? false}
   :strengthen  {:label "Strengthen"  :amount? false}
   :add-target  {:label "Add Target"  :amount? false}})

(defn effect-label
  "Display text for an attached effect -- \"Push 2\", \"Stun\" -- nil for
   no effect at all. No amount is ever shown for a flag-only effect, even
   if one was somehow stored alongside it."
  [effect amount]
  (if-let [{:keys [label amount?]} (get effect-kinds effect)]
    (if (and amount? amount) (str label " " amount) label)))
