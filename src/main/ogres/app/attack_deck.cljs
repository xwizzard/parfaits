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
  {:minus-2 -2 :minus-1 -1 :plus-0 0 :plus-1 1 :plus-2 2 :plus-3 3 :plus-4 4
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
   :heal        {:label "Heal"        :amount? true :signed? true}
   :stun        {:label "Stun"        :amount? false}
   :disarm      {:label "Disarm"      :amount? false}
   :muddle      {:label "Muddle"      :amount? false}
   :immobilize  {:label "Immobilize"  :amount? false}
   :poison      {:label "Poison"      :amount? false}
   :wound       {:label "Wound"       :amount? false}
   :invisible   {:label "Invisible"   :amount? false}
   :strengthen  {:label "Strengthen"  :amount? false}
   ;; Frosthaven's two additions. Both already existed as token badges with
   ;; their own traced glyph and colour -- the only thing that had ever been
   ;; missing was an entry here, because this vocabulary was read off the
   ;; GLOOMHAVEN class decks and neither appears in one.
   :regenerate  {:label "Regenerate"  :amount? false}
   :brittle     {:label "Brittle"     :amount? false}
   :add-target  {:label "Add Target"  :amount? true :signed? true}
   ;; Needed by the Gloomhaven class decks (see gloomhaven-classes):
   ;; infusing an element, applying CURSE as an effect rather than as a
   ;; card of its own, and refreshing a spent item.
   :element     {:label "Element"     :amount? true}
   :curse       {:label "Curse"       :amount? false}
   :refresh-item {:label "Refresh Item" :amount? false}})

(defn effect-label
  "Display text for an attached effect -- \"Push 2\", \"Stun\" -- nil for
   no effect at all. No amount is ever shown for a flag-only effect, even
   if one was somehow stored alongside it."
  [effect amount]
  (if-let [{:keys [label amount?]} (get effect-kinds effect)]
    (if (and amount? amount)
      ;; An element's amount is WHICH element, not how much of it.
      (str label " " (if (keyword? amount) (name amount) amount))
      label)))

;; --- Card faces ---
;; The printed cards are one flat field carrying a centred medallion. A
;; card has no identity of its own -- which deck it belongs to lives on
;; the deck, not the card -- so a face is assembled from at most five
;; components: a fill, a modifier (a numeral or a glyph), an optional
;; attached effect, an optional pair of wings, and the shuffle mark.

(def ^:private effect-icons
  "The glyph each attached effect shows in the medallion. The first six
   were traced for this feature; the rest reuse the condition icons
   already in icons.svg, which are the same symbols the printed cards
   use for these effects."
  {:push       "am-push"
   :pull       "am-pull"
   :pierce     "am-pierce"
   :shield     "am-shield"
   ;; The solid drop, not the crossed one: a number sits over this glyph,
   ;; and the cross knocked out of the middle is exactly where it lands.
   :heal       "am-heal-solid"
   :add-target "am-target"
   :stun       "condition-stun"
   :disarm     "condition-disarm"
   :muddle     "condition-muddle"
   :immobilize "condition-immobilize"
   :poison     "condition-poison"
   :wound      "condition-wound"
   :invisible  "condition-invisible"
   :strengthen "condition-strengthen"
   :regenerate "condition-regenerate"
   :brittle    "condition-brittle"
   :curse      "am-curse"
   :refresh-item "am-item"})

(def effect-colors
  "The accent each effect carries on a card -- the fill behind its glyph.

   The statuses use the exact colours the token badges use (see
   game_type/games/gloomhaven's :badge-color), because both were sampled
   from the same reference art; a test asserts they stay equal rather
   than drifting apart. The rest were sampled from the printed perk
   cards' own diamonds.

   No accent carries an ink flag any more. It only ever marked the pale
   element accents, and no path draws a glyph on one: an element sits on
   the bare card, and every other effect's accent is a mid tone that white
   reads on. A flag nothing renders is a value nothing checks -- DARK's was
   silently wrong for three commits before anyone looked at it.

   :field is the colour the WHOLE card takes when the effect owns the
   medallion (see `card-face`) -- the printed cards recolour the field to
   the effect rather than leaving it the neutral brown of the +0 it is
   printed on. It is the accent's own hue and chroma held at one
   lightness, because the printed fields sit in a single tonal band no
   matter how bright or dark the accent itself is: a wound card's field
   is mid-orange, not the near-white of its diamond. INVISIBLE is the one
   exception the printed cards make, and they make it deliberately --
   darkness is that condition's whole meaning, so its field stays dark.

   HEAL is the one effect that carries no :field at all -- it keeps
   painting its own :color behind the glyph instead, on a fixed neutral
   background (see `neutral-field`) rather than the effect-derived wash
   every other card gets. Measured on the printed Frosthaven cards: a
   rolling +0 heal sits on the same parchment brown a plain +0 does,
   with its own red disc still behind the drop -- a genuine two-tone
   card, not a rounding error.

   SHIELD looks like it belongs in that exception alongside heal -- its
   accent is a muted brown too -- but measured against the printed
   Frosthaven shield cards, the accent and the surrounding field are the
   SAME colour: there is no second tone to see. Its :field is that same
   accent at the common lightness, same as every other effect, and the
   diamond goes plain like theirs; painting a second brown disc under a
   background that is already that brown was never a two-tone card, only
   a redundant one."
  {:stun         {:color "#2b4265" :field "oklch(0.47 0.067 258.5)"}
   :disarm       {:color "#68787d" :field "oklch(0.47 0.021 218.5)"}
   :muddle       {:color "#725945" :field "oklch(0.47 0.045 60.1)"}
   :immobilize   {:color "#9a322d" :field "oklch(0.47 0.130 26.7)"}
   :poison       {:color "#7c8167" :field "oklch(0.47 0.039 116.4)"}
   :wound        {:color "#e56225" :field "oklch(0.47 0.130 42.2)"}
   :invisible    {:color "#131413" :field "oklch(0.26 0.003 145.5)"}
   :strengthen   {:color "#4a98d4" :field "oklch(0.47 0.118 244.3)"}
   :regenerate   {:color "#c73b96" :field "oklch(0.47 0.130 345.0)"}
   :brittle      {:color "#2798a3" :field "oklch(0.47 0.097 204.8)"}
   :curse        {:color "#7e58a6" :field "oklch(0.47 0.124 305.1)"}
   :push         {:color "#515254" :field "oklch(0.47 0.004 264.5)"}
   :pull         {:color "#4e4f51" :field "oklch(0.47 0.004 264.5)"}
   :pierce       {:color "#cf9353" :field "oklch(0.47 0.109 66.3)"}
   :shield       {:color "#73513b" :field "oklch(0.47 0.057 53.8)"}
   :heal         {:color "#b02925"}
   :add-target   {:color "#ab1e23" :field "oklch(0.47 0.130 25.6)"}
   :refresh-item {:color "#a05a42" :field "oklch(0.47 0.099 39.6)"}})

(def element-colors
  "Elements colour by which element, like their glyphs do.

   Sampled from the medallions of four printed decks whose element blocks
   were read off a contact sheet rather than derived: the Elementalist's
   fire/ice/air/earth, the Sunkeeper's light, and the Deathwalker's dark.

   :color is the orb the glyph sits in and keeps the brightness it was
   measured at -- it is the one place on our cards where a colour is drawn
   at its own lightness rather than the deck's common one, because on the
   printed card it is the bright thing the eye lands on.

   :field is that colour's hue and chroma at the common lightness, so the
   card reads as a darker wash of its own element, which is the two-tone
   arrangement the printed card has."
  {:fire  {:color "#e45626" :field "oklch(0.47 0.130 37.8)"}
   :ice   {:color "#34c2f0" :field "oklch(0.47 0.130 225.1)"}
   :air   {:color "#9cb1bd" :field "oklch(0.47 0.029 232.2)"}
   :earth {:color "#88a63f" :field "oklch(0.47 0.130 123.5)"}
   :light {:color "#f4ae1c" :field "oklch(0.47 0.130 78.2)"}
   :dark  {:color "#163856" :field "oklch(0.47 0.067 248.7)"}})

(def ^:private target-labels
  "How an effect's target reads on the card."
  {:self "Self" :ally "Ally"})

(def ^:private element-caption
  "What an element card DOES, spelled out beneath the glyph. A bare
   element symbol says which element without saying what happens to it,
   and this is the one effect whose glyph is a noun rather than a verb."
  "Create")

(def ^:private element-icons
  "Which element an :element effect infuses. The amount carries the
   element rather than a quantity, so the glyph comes from it."
  {:fire "am-fire" :ice "am-ice" :air "am-air"
   :earth "am-earth" :light "am-light" :dark "am-dark"})

(def ^:private faces
  "What each kind's medallion carries, and the fill it sits on.

   BLESS is a 2x card and CURSE is a Null card -- the blessing/curse mark
   is secondary, riding in the wings rather than displacing the modifier
   (which is also why `values` gives them the same sentinels). Only Null
   and 2x carry the shuffle mark; the one-shots are removed from the deck
   when drawn rather than reshuffled."
  {:minus-2 {:fill :negative :value "-2"}
   :minus-1 {:fill :negative :value "-1"}
   :plus-0  {:fill :neutral  :value "+0"}
   :plus-1  {:fill :positive :value "+1"}
   :plus-2  {:fill :positive :value "+2"}
   :plus-3  {:fill :positive :value "+3"}
   :plus-4  {:fill :positive :value "+4"}
   :null    {:fill :null     :glyph "am-null" :shuffle? true}
   :times-2 {:fill :double   :value "2x"      :shuffle? true}
   :bless   {:fill :double   :value "2x"      :wings :bless :wing-glyph "am-bless"}
   :curse   {:fill :null     :glyph "am-null" :wings :curse :wing-glyph "am-curse"}})

(def ^:private reduced-faces
  "Reduced Randomness (p.49) flattens the four extreme kinds to a plain
   +2/-2. The face takes the matching SIGN fill as well as the value --
   a purple card reading '+2' would be a puzzle -- while keeping its
   wings and shuffle mark, since neither the one-shot nor the reshuffle
   behaviour changes under the variant."
  {:times-2 {:fill :positive :value "+2"}
   :bless   {:fill :positive :value "+2"}
   :null    {:fill :negative :value "-2"}
   :curse   {:fill :negative :value "-2"}})

(defn ^:private amount-caption
  "The phrase an effect's quantity reads as, or nil when the quantity
   stands on its own beneath the glyph.

   A lone 1 under an arrow could be a distance, a number of targets, or
   damage; \"Push 1\" says which. Add Target reads sign-first, the way the
   printed card writes it."
  [effect amount]
  (case effect
    (:push :pull :pierce) (str (:label (get effect-kinds effect)) " " amount)
    :add-target           (str "+" amount " Target")
    nil))


(def neutral-field
  "The plain parchment brown Heal sits on once the effect owns the
   medallion (see `card-face`) -- not derived from its own accent (a red),
   because the printed Frosthaven card shows a red disc on a brown field,
   two tones, not one. Fixed rather than per-kind, since the background
   does not change with the modifier either: a +0 rolling heal and a +1
   heal sit on the same brown. The same tone a plain +0 is printed on,
   which is what makes it read as neutral rather than as a colour choice
   of its own."
  "oklch(0.52 0.03 60)")

(defn card-face
  "The components of one card's face, as a map:

     :fill        one of :positive :negative :neutral :null :double
     :value       the modifier as text, or nil if it is a glyph
     :glyph       the modifier as an icon name, or nil if it is text
     :effect-icon the attached effect's glyph, when the card has one --
                  the effect always owns the medallion when present (see
                  below), so this is also what tells the caller whether
                  :value/:glyph belong there or in the modifier's chip
     :color       that effect's accent, for the fill behind the glyph --
                  absent when the effect has taken the whole card instead
     :field-color the colour the CARD takes: the effect's own where it has
                  one, otherwise (Heal alone) a fixed neutral
     :element-color the orb an element's glyph sits in, at the brightness
                  the printed card gives it
     :plain-medallion? true when the glyph sits straight on the field,
                  with no accent drawn behind it
     :amount      the effect's quantity, when it has one
     :amount-label that quantity as it should read, signed where the
                  effect adds rather than reduces
     :target      who the effect applies to, when the card says
     :caption     the word beneath the glyph -- a target (\"Self\"), a
                  phrase (\"Push 1\"), an element's verb (\"Create\"), or a
                  condition's own name
     :rolling?    whether the card is a rolling modifier -- it resolves and
                  the draw continues, rather than ending it
     :wings       :bless or :curse, for the two one-shot kinds
     :wing-glyph  the identity mark carried in those wings
     :shuffle?    whether the card triggers a reshuffle when drawn

   A card carrying BOTH a modifier and an effect gives the medallion to
   the EFFECT and demotes the modifier -- whatever its own kind, including
   a bare +0 -- to a small chip in its own sign colour. This is
   Frosthaven's printed arrangement, and it turned out to be the one to
   build toward: Gloomhaven's own cards (modifier big, effect small) are
   the earlier of the two, checked against Frosthaven's own paired class
   decks once those existed to check against. The caller decides where
   each component is drawn; this only says which components exist."
  ([kind]
   (card-face kind nil))
  ([kind {:keys [effect amount rolling? target reduced?]}]
   (let [base (get faces kind {:fill :neutral :value (str kind)})
         base (if reduced? (merge base (get reduced-faces kind)) base)
         ;; A reduced face replaces Null's glyph with a numeral, so drop
         ;; any glyph the base kind had once a value is present.
         base (if (:value base) (dissoc base :glyph) base)
         face
         (cond-> base
           ;; An element's glyph depends on WHICH element, which rides in
           ;; the amount rather than in the effect itself.
           (= effect :element)
           (merge {:effect-icon (get element-icons amount "am-fire")
                   :caption element-caption}
                  (get element-colors amount))

           (and (not= effect :element) (contains? effect-icons effect))
           (merge {:effect-icon (get effect-icons effect)}
                  ;; An effect that takes no quantity leaves the caption
                  ;; slot empty, so it spends it naming itself. Two
                  ;; conditions can look alike at panel size -- poison and
                  ;; wound are both a dark mark on a pale field -- and the
                  ;; word settles it without the reader learning a glyph.
                  ;; The ones that DO carry a number keep it there; a
                  ;; number is the fact you cannot infer from the symbol.
                  (if (not (:amount? (get effect-kinds effect)))
                    {:caption (:label (get effect-kinds effect))})
                  (get effect-colors effect))

           ;; Shield 1, Pierce 3, Push 1 -- the quantity belongs with the
           ;; glyph. An element's "amount" is which element, not how much,
           ;; so it never renders as a number.
           (and effect (not= effect :element) (number? amount))
           (assoc :amount amount
                  ;; Heal and Add Target ADD -- they read "+1", where
                  ;; Pierce reduces and reads a plain "3".
                  :amount-label (str (if (:signed? (get effect-kinds effect)) "+") amount))

           ;; ...unless the quantity reads better as a phrase, in which
           ;; case it takes the caption slot and leaves the number slot
           ;; empty rather than saying the same thing twice.
           (and effect (number? amount) (amount-caption effect amount))
           (-> (assoc :caption (amount-caption effect amount))
               (dissoc :amount-label))

           rolling?
           (assoc :rolling? true)

           ;; The qualifier the printed cards write beneath the glyph --
           ;; every heal and shield card in the class decks is Self. It
           ;; shares the caption slot with the element verb above, and
           ;; wins it: a stated target is more specific than "Create".
           (and effect target)
           (assoc :target target
                  :caption (get target-labels target (name target))))]
     (if (:effect-icon face)
       ;; The accent is read here, before anything below might dissoc it.
       (let [accent (:color face)]
         (-> face
             (assoc :field-color (or (:field face) neutral-field))
             (cond->
               ;; The card is already painted the effect's colour, so a
               ;; diamond in that same colour would only be a seam -- goes
               ;; plain, and its own accent goes with it. Heal alone has no
               ;; :field, so this never fires for it: it keeps painting
               ;; `accent` behind the glyph, which is what tells it apart
               ;; from its own neutral background.
               (:field face) (-> (assoc :plain-medallion? true) (dissoc :color))
               (= effect :element) (assoc :element-color accent))
             (dissoc :field)))
       (dissoc face :field)))))
