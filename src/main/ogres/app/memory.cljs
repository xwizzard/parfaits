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
  "The four suits, in the order the standard-52 deck definition lists
   them (game-type/core-decks) so a Memory board and a dealt deck read
   the same way."
  [:clubs :diamonds :hearts :spades])

(def ranks
  [:ace :two :three :four :five :six :seven :eight :nine :ten
   :jack :queen :king])

(def deck-size
  "Cards in a full deal -- the whole standard deck."
  (* (count suits) (count ranks)))

(defn table-footprint
  "The unscaled [width height] of a table, whatever it currently holds.

   Deliberately NOT a function of the cards still in play: a table is
   placed empty and later dealt into, and matched pairs leave the board
   as the game runs. Sizing the felt to the live card count would grow
   the table the moment a game started and shrink it under the players
   with every pair they found."
  []
  (table-size deck-size))

(def ^:private red-suits #{:hearts :diamonds})

(defn suit-color
  "A suit's colour, :red or :black. This is half of what makes a pair --
   see `pair?`."
  [suit]
  (if (contains? red-suits suit) :red :black))

(defn pair?
  "True if two cards match. A single deck holds only one of each exact
   card, so pairs are matched on RANK AND COLOUR: the two black kings
   make a pair, as do the two red kings. That partitions all 52 cards
   into exactly 26 pairs with nothing left over, which is why the whole
   deck is dealt rather than a subset."
  [a b]
  (and (= (:card/rank a) (:card/rank b))
       (= (suit-color (:card/suit a)) (suit-color (:card/suit b)))
       (not= (:card/suit a) (:card/suit b))))

(defn deal
  "A shuffled standard 52-card deck in grid order, index 0 first -- a
   vector of {:card/rank :card/suit} maps. Position is not dealt: a
   card's index in this vector IS its grid slot, and card-offset turns
   that into coordinates at render time."
  []
  (shuffle
   (into [] (for [suit suits rank ranks] {:card/rank rank :card/suit suit}))))
