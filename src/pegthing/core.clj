(ns pegthing.core
  (:require [clojure.string :as s])
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

(declare prompt-rows)

(defn -main [] (prompt-rows))

;; ---------------------------------
;; maintain game state
;; ---------------------------------

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
    (reduce (fn [new-board create-connection-fn]
              (create-connection-fn new-board pos))
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

(defn valid-move?
  "Return jumped position if the move from p1 to p2 is valid,
   otherwise return nil"
  [board p1 p2]
  (get (valid-moves board p1) p2))

(defn make-move
  "Move peg from p1 to p2, removing jumped peg"
  [board p1 p2]
  (if-let [jumped (valid-move? board p1 p2)]
    (move-peg (remove-peg board jumped) p1 p2)
    nil))

(defn pegged-positions
  "Return a sequence of pegged positions on the board"
  [board]
  (map first (filter #(:pegged (second %)) board)))

;; the high-level goal here is to check whether at least one
;; position exists where :pegged = true and valid-moves
;; returns a non-empty map
;;
;;     (comp not-empty (partial valid-moves board))
;;
;; is equivalent to
;;
;;     (fn [pos] (not-empty (valid-moves board pos)))
;;
(defn can-move?
  "Are there any valid moves available on the board?
   If so, returns the first available (valid) move."
  [board]
  (some (comp not-empty (partial valid-moves board))
        (pegged-positions board)))

;; ---------------------------------
;; render the board
;; ---------------------------------

(def alpha-start 97)               ; a
(def alpha-end (+ alpha-start 26)) ; z
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3) ; the width of one position when rendered

(def esc \u001b)
(def ansi-colors {:reset "[0m"
                  :dim "[2m"
                  :red "[31m"
                  :green "[32m"
                  :blue "[34m"})

(defn ansi
  [color]
  (str esc (color ansi-colors)))

(defn colorize
  "Wrap the input string with ANSI escape codes to generate
   colored text when output to a terminal."
  [color input]
  (str (ansi color) input (ansi :reset)))

(defn render-pos
  "Render a single position on the board"
  [board pos]
  (str (nth letters (dec pos))
       (if (get-in board [pos :pegged])
         (colorize :blue "o")
         (colorize :red "-"))))

;; start from one greater than the highest position in the row above
;; (or 0 if we're on the top row), then proceed through the highest
;; position in this row (plus 1, to account for the fact that ranges
;; are end-exclusive)
(defn row-positions
  "Return all positions in the given row"
  [row]
  (range (inc (or (row-tri (dec row)) 0))
         (inc (row-tri row))))

(defn row-padding
  "String of spaces to add to the beginning of a row to center it."
  [row total-rows]
  (let [pad-length (/ (* (- total-rows row) pos-chars) 2)]
    (apply str (take pad-length (repeat " ")))))

;; as before, we can rewrite
;;
;;     (partial render-pos board)
;;
;; as
;;
;;     #(render-pos board %)
;;
(defn render-row
  [board row]
  (str (row-padding row (:rows board))
       (s/join " " (map (partial render-pos board)
                        (row-positions row)))))

(defn print-board
  [board]
  (doseq [row (range 1 (inc (:rows board)))]
    (println (render-row board row))))

;; ---------------------------------
;; player interaction
;; ---------------------------------

(defn letter->pos
  "Converts a letter string to the corresponding position
   on the board"
  [letter]
  (inc (- (int (first letter)) alpha-start)))

(defn get-input
  "Waits for user to enter text and hit enter, then cleans
   the input"
  ([] (get-input nil))
  ([default]
   (let [input (s/trim (read-line))]
     (if (empty? input)
       default
       (s/lower-case input)))))

(defn characters-as-strings
  "Given an input string containing any number of characters,
   return a sequence of single-character strings with all
   nonalphabetic input discarded."
  [input]
  (map str (s/replace input #"[^A-Za-z0-9]" "")))

(declare user-entered-valid-move user-entered-invalid-move game-over)

(defn prompt-move
  [board]
  (println "\nHere's your board:")
  (print-board board)
  (println "Move from where to where? Enter two letters:")
  (let [input (map letter->pos (characters-as-strings (get-input)))]
    (if-let [new-board (make-move board (first input) (second input))]
      (user-entered-valid-move new-board)
      (user-entered-invalid-move board))))

(defn user-entered-valid-move
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))

(defn user-entered-invalid-move
  [board]
  (println "\n!!! That move isn't valid :(\n")
  (prompt-move board))

(defn prompt-empty-peg
  [board]
  (println "Here's your board:")
  (print-board board)
  (println "Remove which peg? [e]")
  (let [new-board (remove-peg board (letter->pos (get-input "e")))]
    (if (can-move? new-board)
      (prompt-move new-board)
      (game-over new-board))))

(defn prompt-rows
  []
  (println "How many rows? [5]")
  (let [rows (Integer. (get-input 5))]
    (if (> rows 3)
      (prompt-empty-peg (new-board rows))
      (do
        (println "Must be greater than 3")
        (prompt-rows)))))

(defn describe-score
  [remaining]
  (case remaining
    0 "Neo, is that you?" ; this shouldn't be possible!!
    1 "*bow* we will follow your career with great interest"
    2 "dang, _that_ close"
    3 "not bad!"
    4 "could be better, could be worse"
    5 "I've seen worse, I guess"
    "...yikes."))

(defn print-score
  [board]
  (let [remaining (count (pegged-positions board))]
    (println "Game over! You had" (colorize :green remaining) "pegs left.")
    (println (colorize :dim (describe-score remaining)))))

(defn game-over
  [board]
  (print-score board)
  (print-board board)
  (println "Play again? y/n [y]")
  (let [input (get-input "y")]
    (if (= "y" input)
      (prompt-rows)
      (do
        (println "Thanks for playing!")
        (System/exit 0)))))
