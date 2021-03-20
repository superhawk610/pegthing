(ns pegthing.core
  (:require [clojure.pprint :as pp])
  (:require [clojure.set :as set])
  (:gen-class))

;; A game board is composed of a triangular number of pegs, laid
;; out as a pyramid, something like:
;; 
;;           (1)
;;         (2) (3)
;;       (4) (5) (6)
;;     (7) (8) (9) (10)
;; 
;; The board is represented using a map of peg positions
;; to peg state; each position either contains a peg (is "pegged")
;; or doesn't, and has some number of connections to other
;; positions. A connection describes both the position that
;; a legal jump points to, as well as the position being jumped
;; over (that would be removed once the jump is completed).
;; 
;; Additionally, the map stores the number of rows under the `:rows`
;; keyword.
;; 
;;     {1 {:pegged true, :connections {6 3, 4 2}},
;;        {:pegged false, :connections {9 5, 7 4}},
;;        ...
;;        :rows 5
;;        :max-pos 15}
;;

(declare successful-move prompt-move game-over query-rows)

(defn -main
  "Launch a new game"
  [] "this should probably do something")

(defn tri*
  "Generates lazy sequence of triangular numbers
   1 (1)
   3 (1 + 2)
   6 (1 + 2 + 3)
   ..."
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

;; bind lazy sequence to a symbol
(def tri (tri*))

(defn triangular?
  "Is the number triangular? (1, 3, 6, 10, 15, ...)
   Also answers the question: Is the number at the end of a row?"
  [n]
  (= n (last (take-while #(>= n %) tri))))

(defn row-tri
  "The triangular number at the end of row n"
  [n]
  (last (take n tri)))

(defn row-num
  "Returns row number the pos belongs to (pos 1 in row 1,
   pos 2 and pos 3 in row 2, ...)"
  [pos]
  (inc (count (take-while #(> pos %) tri))))

;; Imagine a jump: (pos) ----(neighbor)---> (destination)
;; 
;; max-pos is only required to validate that the jump
;; can take place, and wouldn't leave the peg out of bounds
(defn connect
  "Form a mutual connection between two positions"
  [{:keys [max-pos] :as board} pos neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos destination] [destination pos]])
    board))

;; given the jump (1) ----(2)---> (3), it's valid as long as the
;; following are all true
;; 
;; - (1) isn't the end of a row
;; - (2) isn't the end of a row
;; 
;; It's OK for (3) to be at the end of a row!
(defn connect-right
  "Form a connection with the pos 2 to the right (if valid)"
  [board pos]
  (let [neighbor (inc pos)          ; pos 1 to the right
        destination (inc neighbor)] ; pos 2 to the right
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board pos neighbor destination)
      board)))

;; given a position, you can calculate the next two positions
;; moving down and to the left using the following formula:
;; 
;; - (1) starting position
;; - (2) down and to the left from (1)
;; - (3) down and to the left from (2)
;;
;; - (1) given
;; - (2) = row( (1) ) + (1)
;; - (3) = row( (1) ) + (2) + 1
;; 
;; here are a couple examples, because (IMO) this isn't very
;; intuitive:
;; 
;; - given pos                  1
;; - neighbor is        1 + 1 = 2
;; - destination is 1 + 2 + 1 = 4
;; 
;; - given pos                  3
;; - neighbor is        2 + 3 = 5
;; - destination is 2 + 5 + 1 = 8
;;
(defn connect-down-left
  "Form a connection with the pos 2 down and to the left (if valid)"
  [board pos]
  (let [row (row-num pos)
        neighbor (+ row pos)
        destination (+ row neighbor 1)]
    (connect board pos neighbor destination)))

(defn connect-down-right
  "Form a connection with the pos 2 down and to the right (if valid)"
  [board pos]
  (let [row (row-num pos)
        neighbor (+ row pos 1)
        destination (+ row neighbor 2)]
    (connect board pos neighbor destination)))

(defn add-pos
  "Pegs a position and performs all valid connections"
  [board pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board create-connection]
              (create-connection new-board pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))

(defn new-board
  "Creates a new board with the given number of rows"
  [rows]
  (let [max-pos (row-tri rows)
        initial-board {:rows rows :max-pos max-pos}]
    (reduce (fn [board pos] (add-pos board pos))
            initial-board
            (range 1 (inc max-pos)))))

(defn pegged?
  "Does the position have a peg in it?"
  [board pos]
  (get-in board [pos :pegged]))

(defn remove-peg
  "Take the peg at the given position out of the board"
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Put a peg in the board at the given position"
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "Take a peg out of p1 and place it in p2"
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))

(defn valid-moves
  "Return a map of all valid moves for pos, where the key is
   the destination and the value is the jumped position
   (the same key:value mapping used when describing connections)"
  [board pos]
  (into {}
        (filter (fn [[destination jumped]]
                  (and (not (pegged? board destination)) ; no peg in destination
                       (pegged? board jumped)))          ; and a peg to hop over
                (get-in board [pos :connections]))))

(comment
  (def board (new-board 5))
  (doseq [pos (range 1 (inc (:max-pos board)))]
    (pp/pprint (valid-moves board pos))))