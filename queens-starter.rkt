;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname queens-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require racket/list)
;; 4 Queens solver

;;The four queens problem consists of finding a way to place
;;four chess queens on a 4 by 4 chess board while making sure 
;;that none of the queens attack each other.

;;==============
;;Data Definitions

;;
;;Board is a list of Boolean
;;interp.
;;visually the board is 4x4 array of squares where each square
;;  has a row and column number (r, c).  But we represent it as a
;;  single flat list, in which the rows are layed out one after
;;  another in a linear fashion. (See interp. of Pos below for how
;;  we convert back and forth between (r, c) and position in a board.)

;; Pos is Natural[0, 16]
;; interp.
;;  the position of a square on the board, for a given p, then
;;    - the row    is (quotient p 4)
;;    - the column is (remainder p 4)

;; Row is Natural[0, 4]
;;interp.
;;  the number of rows that the board has.

;; Col is Natural[0, 4]
;;interp.
;;  the number of columns that the board has.


;; Convert 0-based row and column to Pos
(define (r-c->pos r c) (+ (* r 4) c))  ;helpful for writing tests

;;===============
;; Constants

(define B false) ;B stands for blank

(define Q true) ;Q stands for Queen

(define BD1       ;empty board 
  (list B B B B
        B B B B
        B B B B
        B B B B))

(define BD2       ;board with one Queen
  (list Q B B B
        B B B B
        B B B B
        B B B B))

(define BD3       ;invalid board 
  (list Q B B B
        B Q B B
        B B B B
        B B B B))

(define BD4       ;valid board
  (list Q B B B
        B B B B
        B Q B B
        B B B B))

(define BD5 
  (list Q B B B     ;invalid board
        B B Q B
        B B B Q
        B Q B B))

(define BDs       ;solved board
  (list B Q B B
        B B B Q
        Q B B B
        B B Q B))

;; Positions of all the rows, columns and diagonals, that queen attacks

(define ROWS
  (list (list  0  1  2  3  )
        (list  4  5  6  7  )
        (list  8  9  10 11 )
        (list  12 13 14 15 )))

(define COLS
  (list (list  0  4  8  12  )
        (list  1  5  9  13  )
        (list  2  6  10 14 )
        (list  3  7  11 15 )))

(define DIAG-RIGHT
  (list (list 0 5 10 15)
        (list 1 6 11)
        (list 2 7)
        (list 4 9 14)
        (list 8 13)))

(define DIAG-LEFT
  (list (list 12 9 6 3)
        (list 13 10 7)
        (list 14 11)
        (list 8 5 2)
        (list 4 1)))

(define ATTACKS (append ROWS COLS DIAG-RIGHT DIAG-LEFT))

;;=============
;; Functions

;;Board -> Board
;;produce a solution of board
(check-expect (queens BD1) BDs)

;(define (queens bd) BD1) ;stub

(define (queens bd)
  (local [(define (solve--bd bd)
            (if(solved? bd)
               bd
               (solve--lob (next-boards bd))))
          
          (define (solve--lob lob)
            (cond [(empty? lob) false]
                  [else
                   (local [(define try (solve--bd (first lob)))] ;try first child
                     (if (not (false? try))                     ;successful?
                         try                                    ;if so produce that
                         (solve--lob (rest lob))))]))]          ;or try rest of children
    
    (solve--bd bd)))

;;Board -> Boolean
;;produce true if the board is solved
;;the board is solved if it has 4 Queens
;;ASSUME the board is valid
(check-expect (solved? BD1) false)
(check-expect (solved? BDs) true)

;(define (solved? bd) false) ;stub

(define (solved? bd)
  (local[(define (sum-queens c1 c2)
           (if c1
               (+ 1 c2)
               c2))]
    (=(foldr sum-queens 0 bd) 4)))

;;Board -> (listof Board)
;;produce the next list of valid board adding a queen in different positions
(check-expect (next-boards BD4) (list (list Q B B B
                                            B B B Q
                                            B Q B B
                                            B B B B)))

;(define (next-boards bd) empty) ;stub

(define (next-boards bd)
  (keep-only-valid(add-queen-boards bd (empty-squares bd))))

;;Board -> (listof Pos)
;;get a list of empty squares positions of the Board
(check-expect (empty-squares BD4) (list 1 2 3 4 5 6 7 8 10 11 12 13 14 15))

;(define (empty-squares bd) empty) ;stub
(define (empty-squares bd)
  (cond[(empty? bd) empty]
       [else
        (if(false? (first bd))
           (cons (- 16 (length bd)) (empty-squares (rest bd)))
           (empty-squares(rest bd)))])) 
           

;;Board (listof positions) -> (listof boards)
;;it generates a list of boards, each board with one queen added in each empty position
(check-expect (add-queen-boards BD4 (list 1 2 3 4 5 6 7 8 10 11 12 13 14 15)) 
                                  (list (list Q Q B B
                                              B B B B
                                              B Q B B
                                              B B B B)
                                        (list Q B Q B
                                              B B B B
                                              B Q B B
                                              B B B B)
                                        (list Q B B Q
                                              B B B B
                                              B Q B B
                                              B B B B)
                                        (list Q B B B
                                              Q B B B
                                              B Q B B
                                              B B B B)
                                        (list Q B B B
                                              B Q B B
                                              B Q B B
                                              B B B B)
                                        (list Q B B B
                                              B B Q B
                                              B Q B B
                                              B B B B)
                                        (list Q B B B
                                              B B B Q
                                              B Q B B
                                              B B B B)
                                        (list Q B B B
                                              B B B B
                                              Q Q B B
                                              B B B B)
                                        (list Q B B B
                                              B B B B
                                              B Q Q B
                                              B B B B)
                                        (list Q B B B
                                              B B B B
                                              B Q B Q
                                              B B B B)
                                        (list Q B B B
                                              B B B B
                                              B Q B B
                                              Q B B B)
                                        (list Q B B B
                                              B B B B
                                              B Q B B
                                              B Q B B)
                                        (list Q B B B
                                              B B B B
                                              B Q B B
                                              B B Q B)
                                        (list Q B B B
                                              B B B B
                                              B Q B B
                                              B B B Q))) 

;(define (add-queen-boards bd) empty) ;stub

(define (add-queen-boards bd lop)
  (cond [(empty? lop) empty]
        [else
           (cons (add-queen-square bd (first lop)) (add-queen-boards bd (rest lop)))])) 

;;(listof Boards) -> (listof Boards)
;; keep only valid boards, filter only valid boards
(check-expect (keep-only-valid (list (list Q Q B B
                                              B B B B
                                              B Q B B
                                              B B B B)
                                        (list Q B Q B
                                              B B B B
                                              B Q B B
                                              B B B B)
                                        (list Q B B Q
                                              B B B B
                                              B Q B B
                                              B B B B)
                                        (list Q B B B
                                              Q B B B
                                              B Q B B
                                              B B B B)
                                        (list Q B B B
                                              B Q B B
                                              B Q B B
                                              B B B B)
                                        (list Q B B B
                                              B B Q B
                                              B Q B B
                                              B B B B)
                                        (list Q B B B
                                              B B B Q
                                              B Q B B
                                              B B B B)
                                        (list Q B B B
                                              B B B B
                                              Q Q B B
                                              B B B B)
                                        (list Q B B B
                                              B B B B
                                              B Q Q B
                                              B B B B)
                                        (list Q B B B
                                              B B B B
                                              B Q B Q
                                              B B B B)
                                        (list Q B B B
                                              B B B B
                                              B Q B B
                                              Q B B B)
                                        (list Q B B B
                                              B B B B
                                              B Q B B
                                              B Q B B)
                                        (list Q B B B
                                              B B B B
                                              B Q B B
                                              B B Q B)
                                        (list Q B B B
                                              B B B B
                                              B Q B B
                                              B B B Q)))
              (list 
               (list Q B B B
                     B B B Q
                     B Q B B
                     B B B B)))

;(define (keep-only-valid lob) empty) ;stub
(define (keep-only-valid lob)
  (filter valid-board? lob))

;;Board -> Boolean
;;check if the board is valid, the board is valid
;;if the queen doesn't attack other queen.
(check-expect (valid-board? BD1) true)
(check-expect (valid-board? BD2) true)
(check-expect (valid-board? BD4) true)
(check-expect (valid-board? BD3) false)
(check-expect (valid-board? BD5) false)

;(define (valid-board? bd) false) ;stub

;(define (valid-board? bd)
;  (local[(define (valid-moves? loa) ;(listof Unit -> Boolean
;           (andmap valid-move? loa))
;         (define (valid-move? u) ;Unit -> Boolean
;           (no-attacks?
;            (keep-only-queens
;             (read-unit u))))
;         
;         (define (read-unit u) ;Unit -> Boolean
;           (map read-pos u)) 
;         
;         (define (read-pos p)  ;Pos -> Boolean
;           (read-square bd p))
;         
;         (define (keep-only-queens lov)
;           (filter identity lov))
;         
;         (define (no-attacks? loa)
;           (< (length loa) 2))]
;         
;    (valid-moves? ATTACKS)))

(define (valid-board? bd)
  (local[(define (get-queens-col lop) ;(listof Pos) -> (listof Col)
           (local[(define (get-col p)
                    (remainder p (sqrt (length bd))))]
           (map get-col lop)))
         
         (define (get-queens-row lop) ;(listof Pos) -> (listof Row)
           (local[(define (get-row p)
                    (quotient p (sqrt (length bd))))]
           (map get-row lop)))
         
         (define lop (get-queens-pos bd))
         
         (define (queen-attack? loc lor) ;(listof col) (listof row) -> Boolean
             (and (no-attacks? loc)(no-attacks? lor)(not(diag-attacks? loc lor))))
         
         (define (no-attacks? locr)  ;(listof Integers) -> Boolean
           (cond[(empty? locr) true]
                [else
                 (if(member (first locr) (rest locr))
                    false
                    (no-attacks?(rest locr)))]))]
    
    (queen-attack? (get-queens-col lop) (get-queens-row lop))))

;;Board -> (listof Pos)
;;get list of queens positions in the board
(check-expect (get-queens-pos BD2) (list 0))

;(define (get-queens-pos bd) empty) ;stub

(define (get-queens-pos bd)
           (cond[(empty? bd) empty]
                [else
                 (if(first bd)
                    (cons (- 16 (length bd)) (get-queens-pos (rest bd)))
                    (get-queens-pos (rest bd)))]))

;;(listof col) (listof lor) -> Boolean
;;check in both list if there is a diagonal attack
(check-expect (diag-attacks? (list 0 1) (list 1 2)) true)
(check-expect (diag-attacks? (list 0 1 2) (list 1 3 4)) true)
(check-expect (diag-attacks? (list 0 1) (list 1 3)) false)

;(define (diag-attacks? loc lor) empty) ;stub


(define (diag-attacks? loc lor)
  (cond[(or(empty? loc)(empty? lor)) false]
       [else
        (if(diag?(first loc)(first lor)(rest loc)(rest lor))
           true
           (diag-attacks? (rest loc) (rest lor)))]))

;;Col Row (listof Col) (listof Row) -> Boolean
;;compare col and row if is diagonal with elements of each list
(check-expect (diag? 0 1 (list 1) (list 2)) true)
(check-expect (diag? 0 1 (list 1) (list 3)) false)
(check-expect (diag? 0 1 empty empty) false)

(define (diag? c1 r1 loc lor)
  (cond[(or(empty? loc)(empty? lor)) false]
       [else
        (if(=(abs(diagonal r1 c1 (first lor) (first loc))) 1)
           true
           (diag? c1 r1 (rest loc) (rest lor)))]))

;; Row Col Row Col -> Integer
;; return the slope of the line between them
(check-expect (diagonal 1 0 2 1) 1)
(check-expect (diagonal 1 0 3 1) 2)
(define (diagonal r1 c1 r2 c2)
  (/(- r2 r1) (- c2 c1)))

;;Board Pos -> Board
;;add queen to square
(check-expect (add-queen-square BD1 0) (cons Q (rest BD1)))

;(define (add-queen-square bd pos) empty) ;stub

(define (add-queen-square bd p)
  (append (take bd p)
          (list Q)
          (drop bd (add1 p))))

;; Board Pos -> Boolean
;; Produce value at given position on board.
(check-expect (read-square BD2 0) true)
(check-expect (read-square BD4 1) false)

(define (read-square bd p)
  (list-ref bd p)) 
