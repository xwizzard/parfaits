(ns ogres.app.memory
  "Pure logic and layout for the 'Memory' example game. No DataScript,
   no UI; mirrors the role ogres.app.cards/ogres.app.props play for
   their own systems. See events.cljs's :memory/* methods,
   component/scene_objects' minigame-table renderer, and
   component/panel_memory.cljs.

   The board is a real standard 52-card deck -- 13 ranks in 4 suits,
   two colours -- dealt face-down into a 13x4 grid. Pairs match on rank
   and colour (see pair?), which is what divides one deck into exactly
   26 pairs.

   A Memory session owns its cards outright (:minigame/cards, a
   :db/isComponent collection) and draws them as one scene object -- a
   `:minigame/table`. Cards are NOT scene props and never appear in
   :scene/props, so nothing in the main game can select, drag, delete or
   sweep them, and tearing the session down cascades for free. An
   earlier version dealt them as 44 individual :prop/prop entities
   borrowed from the scene, which put a running game's board at the
   mercy of every prop-wide action in the app.

   A card carries only its value and its grid index; where it actually
   sits is derived from the table's own :object/point and :object/scale
   (see card-offset), which is what lets the whole board move and scale
   as a single unit."
  (:require [ogres.app.vec :refer [Vec2]]))

(def ^:const card-width
  "Native width of a card face. Faces are drawn inline (see
   component/scene-objects' minigame-card), not loaded as images."
  200)

(def ^:const card-height 280)

(def ^:const card-gap
  "Native gap between adjacent cards, in the same units as card-width."
  40)

(def ^:const columns 13)

(def ^:const table-padding
  "Felt border around the card grid, in the same native units. The felt
   IS the table -- it's what you grab to move the board -- so this is
   part of the object's footprint, not decoration outside it."
  60)

(def ^:const default-scale
  "The table's starting :object/scale. A full deck is 13 columns wide,
   so the board is ~3240 native units across; 0.25 lands that at about
   11 grid cells -- big enough to read a corner index, small enough to
   see the whole table without panning. It is a starting size, not a
   fixed one: the table is scalable (see scale-locked?)."
  0.25)

(defn rows
  "How many grid rows `n-cards` occupies at `columns` wide."
  [n-cards]
  (max 1 (js/Math.ceil (/ n-cards columns))))

(defn card-offset
  "The unscaled offset of the card at grid `index` from the table's own
   origin. Callers scale this by the table's :object/scale -- position is
   never stored per card, which is precisely what makes the board one
   movable, scalable object."
  [index]
  (Vec2. (+ table-padding (* (mod index columns) (+ card-width card-gap)))
         (+ table-padding (* (quot index columns) (+ card-height card-gap)))))

(defn table-size
  "The unscaled [width height] a table of `n-cards` occupies."
  [n-cards]
  (let [cols (min columns (max 1 n-cards))]
    [(+ (* 2 table-padding) (- (* cols (+ card-width card-gap)) card-gap))
     (+ (* 2 table-padding) (- (* (rows n-cards) (+ card-height card-gap)) card-gap))]))

(def suits
  "All four suits. Safe to deal every suit because a match means two
   IDENTICAL cards -- the king of spades pairs only with the other king
   of spades, never with the king of clubs. (Under the old rank-and-
   colour rule those two did match, which is why clubs and diamonds had
   to be dropped; identical-matching removes that constraint.)"
  [:clubs :diamonds :hearts :spades])

(def face-ranks
  "Aces and the court cards -- dealt at every size, so even the smallest
   game has four visually distinct ranks rather than four adjacent pip
   counts that are easy to confuse at a glance."
  [:ace :jack :queen :king])

(def numbered-ranks
  "The pip ranks, added one at a time to make the game harder."
  [:two :three :four :five :six :seven :eight :nine :ten])

(def ranks
  "Every rank a card face may show, in reading order."
  [:ace :two :three :four :five :six :seven :eight :nine :ten
   :jack :queen :king])

(def ^:const copies
  "How many of each card a deal contains. Two, so every card has exactly
   one identical partner."
  2)

(def max-difficulty
  "The hardest game: every numbered rank added on top of the faces."
  (count numbered-ranks))

(def ^:const default-difficulty
  "Two numbered ranks on top of the faces -- 48 cards, which fills the
   same four rows the fixed 52-card deal used to, so the table lands at
   its familiar size unless asked otherwise."
  2)

(defn clamp-difficulty
  [level]
  (max 0 (min max-difficulty (or level default-difficulty))))

(defn ranks-for-difficulty
  "The ranks dealt at `level`: the faces always, plus `level` numbered
   ranks counting up from the two."
  [level]
  (into face-ranks (take (clamp-difficulty level) numbered-ranks)))

(defn deck-size
  "Cards dealt at `level` -- 32 at the easiest (aces and faces alone),
   104 at the hardest (the whole double deck), one numbered rank and so
   8 cards per step between."
  [level]
  (* (count suits) copies (count (ranks-for-difficulty level))))

(defn difficulty
  "A table's chosen size, defaulted."
  [entity]
  (clamp-difficulty (:memory/difficulty entity)))

(defn table-in-play?
  "True when `entity` is a Memory table with cards still on it -- a game
   in progress.

   Such a table is frozen in place: neither movable nor resizable, since
   either would shift the board out from under the players mid-game. It
   thaws once the last pair is claimed and the table is an empty frame
   again, ready to be repositioned or cleared."
  [entity]
  (and (= (:object/type entity) :minigame/table)
       (seq (:minigame/cards entity))))

(defn table-footprint
  "The unscaled [width height] of `entity`, whatever it currently holds.

   Sized from the table's CHOSEN difficulty, not the cards on it: a
   table is placed empty and dealt into later, and matched pairs leave
   the board as the game runs. Sizing the felt to the live card count
   would grow the table the moment a game started and shrink it under
   the players with every pair they found.

   Width is the same at every size -- the grid is always `columns`
   wide -- so making a game harder extends the table downwards only."
  [entity]
  (table-size (deck-size (difficulty entity))))

(def ^:private red-suits #{:hearts :diamonds})

(defn suit-color
  "A suit's colour, :red or :black -- how a card face is drawn. NOT part
   of the match rule any more; see `pair?`."
  [suit]
  (if (contains? red-suits suit) :red :black))

(defn pair?
  "True if two cards show the same face -- same rank, same suit. The deal
   holds exactly two of each card (see `copies`), so this partitions all
   52 into 26 pairs with nothing left over.

   Expects two DIFFERENT cards. Every caller compares two distinct
   face-up cards, and a card cannot be flipped while already face-up, so
   a card is never handed to this alongside itself."
  [a b]
  (and (= (:card/rank a) (:card/rank b))
       (= (:card/suit a) (:card/suit b))))

(defn deal
  "A shuffled deal at `level`, in grid order, index 0 first -- a vector
   of {:card/rank :card/suit} maps, two identical copies of every card
   in play. Position is not dealt: a card's index in this vector IS its
   grid slot, and card-offset turns that into coordinates at render
   time."
  [level]
  (shuffle
   (into [] (for [suit suits rank (ranks-for-difficulty level) _ (range copies)]
              {:card/rank rank :card/suit suit}))))
