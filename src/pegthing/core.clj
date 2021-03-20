(ns pegthing.core
  (:require [clojure.set :as set])
  (:gen-class))

;; TODO: include max-pos in the board?

;; A game board is represented using a map of peg positions
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
;;        :rows 5}
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
  [board max-pos pos neighbor destination]
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
  [board max-pos pos]
  (let [neighbor (inc pos)          ; pos 1 to the right
        destination (inc neighbor)] ; pos 2 to the right
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board max-pos pos neighbor destination)
      board)))
