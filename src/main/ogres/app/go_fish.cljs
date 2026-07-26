(ns ogres.app.go-fish
  "Pure logic for the 'Go Fish' example game -- a demonstration of the
   generic card/deck system's :card/holder-based hands (ogres.app.cards,
   events.cljs's :deck/* methods), not part of the generic engine
   itself. No DataScript, no UI; mirrors the role ogres.app.memory plays
   for its own game. See events.cljs's :go-fish/* methods and
   component/panel_go_fish.cljs. Turn-cycle/winner logic lives in
   ogres.app.turn-order, shared with Memory -- nothing here duplicates
   it.")

(defn cards-of-rank
  "The subset of `cards` (typically one player's hand) whose :card/rank
   is `rank`."
  [cards rank]
  (filter (comp #{rank} :card/rank) cards))

(defn scoreable-count
  "How many of a player's `n` cards of one rank can be laid down as a
   scored set right now, given the active scoring mode -- 0 if not
   (yet) eligible. Book mode requires all 4 at once; pair mode allows
   laying down 2 at a time as soon as a player holds >=2, without
   waiting to collect all 4 -- a 3-of-a-kind lays down 1 pair (2
   cards), keeping the odd card in hand for a future match."
  [n book-scoring?]
  (if book-scoring?
    (if (= n 4) 4 0)
    (* 2 (quot n 2))))
