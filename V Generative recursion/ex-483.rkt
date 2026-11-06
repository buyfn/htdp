#lang htdp/isl+
(require 2htdp/abstraction)

; A QP is a structure:
; (make-posn CI CI)
; A CI is an N in [0, n), where n is the size of a board
; interpretation: (make-posn r c) denotes the square at
; the r-th row and c-th column

; A Board is a structure (make-board N [List-of QP])
; interpretation: (make-board size queens) represents a board of given size
; and a list of positions where a queen has been placed
(define-struct board [size queens])

; N -> [Maybe [List-of QP]]
; finds a solution to the n queens problem
; data example: [List-of QP]
(define 4QUEEN-SOLUTION-1
  (list (make-posn 0 1) (make-posn 1 3)
        (make-posn 2 0) (make-posn 3 2)))
(define 4QUEEN-SOLUTION-2
  (list (make-posn 0 2) (make-posn 1 0)
        (make-posn 2 3) (make-posn 3 1)))
(check-satisfied (n-queens 4) (n-queens-solution? 4))
(define (n-queens n)
  (place-queens (board0 n) n))

; Board N -> [Maybe [List-of QP]]
; places n queens on board; otherwise, returns #false
(define (place-queens a-board n)
  (cond
    [(zero? n) '()]
    [else (place-queens/list (find-open-spots a-board) a-board n)]))

; [List-of QP] Board N -> [Maybe [List-of QP]]
; places n queens on board by trying to place the first queen into
; one of the positions from open-spots; otherwise, returns #false
; Assume n is at least 1
; Termination: each recursive call to place-queens decreases n until it's zero;
; each recursive call to place-queens/list decreases the length of open-spots,
; until it's empty while n stays the same, so only finitely many calls happen
; before n decreases
(define (place-queens/list open-spots a-board n)
  (cond
    [(empty? open-spots) #false]
    [else (local ((define candidate
                    (place-queens (add-queen a-board (first open-spots))
                                  (- n 1))))
            (cond
              [(boolean? candidate)
               (place-queens/list (rest open-spots) a-board n)]
              [else (cons (first open-spots) candidate)]))]))

; N -> Board
; creates the initial n by n board
(define (board0 n)
  (make-board n '()))

; Board QP -> Board
; places a queen at qp on a-board
(define (add-queen a-board qp)
  (make-board (board-size a-board)
              (cons qp (board-queens a-board))))

; Board -> [List-of QP]
; finds spots where it is still safe to place a queen
(check-expect
 (find-open-spots (make-board 2 (list (make-posn 0 0))))
 '())
(check-expect
 (find-open-spots (make-board 3 (list (make-posn 0 0))))
 (list (make-posn 1 2) (make-posn 2 1)))
(define (find-open-spots a-board)
  (local ((define size (board-size a-board))
          (define all-squares
            (for*/list ((row size) (col size))
              (make-posn row col)))
          (define queens (board-queens a-board))
          ; QP -> Boolean
          ; determines whether no queen on the board
          ; threatens given square s
          (define (safe-square? s)
            (andmap (lambda (q) (not (threatening? q s))) queens)))
    (filter safe-square? all-squares)))

; [X] [List-of X] [List-of X] -> Boolean
; determines whether two lists contain the same items,
; regardless of their order
(check-expect (set=? '(1 2 3) '(1 2 3)) #true)
(check-expect (set=? '(1 2 3) '(3 1 2)) #true)
(check-expect (set=? '(1 2 3) '(1 2 3 4)) #false)
(check-expect (set=? '(1 2 4) '(1 2 3)) #false)
(check-expect (set=? '(1 1 2) '(1 2 3)) #false)
(check-expect (set=? '(1 2 3) '(1 1 3)) #false)
(define (set=? s1 s2)
  (and (andmap (lambda (x) (member? x s1)) s2)
       (andmap (lambda (x) (member? x s2)) s1)))

; [List-of QP] -> Boolean
; is the result equal [as a set] to one of two lists
(define (is-queens-result? x)
  (or (set=? 4QUEEN-SOLUTION-1 x)
      (set=? 4QUEEN-SOLUTION-2 x)))

; N -> [[Maybe [List-of QP]] -> Boolean]
; produces a predicate on queen placements that determines
; whether a given placement is a solution to an n queens problem
(define (n-queens-solution? n)
  (lambda (lo-qp)
    (and (list? lo-qp)
         (= (length lo-qp) n)
         (andmap (lambda (p) (not (threatening? (first p) (second p))))
                 (all-pairs lo-qp)))))

; QP QP -> Boolean
; determines whether two queens threaten each other
(check-expect (threatening? (make-posn 3 0) (make-posn 3 7)) #true)
(check-expect (threatening? (make-posn 0 2) (make-posn 7 2)) #true)
(check-expect (threatening? (make-posn 0 0) (make-posn 7 7)) #true)
(check-expect (threatening? (make-posn 0 7) (make-posn 7 0)) #true)
(check-expect (threatening? (make-posn 1 2) (make-posn 4 6)) #false)
(define (threatening? q1 q2)
  (local ((define same-row? (= (posn-x q1) (posn-x q2)))
          (define same-col? (= (posn-y q1) (posn-y q2)))
          (define same-diagonal? (= (abs (- (posn-x q1) (posn-x q2)))
                                    (abs (- (posn-y q1) (posn-y q2))))))
    (or same-row? same-col? same-diagonal?)))

; [X] [List-of X] -> [List-of (list X X)]
; returns a list of all possible pairs
; of the elements of l
(define (all-pairs l)
  (local (; [X] X [List-of X] -> [List-of (list X X)]
          (define (pairs-with-each elt l)
            (map (lambda (x) (list elt x)) l)))
    (cond
      [(< (length l) 2) '()]
      [else (append (pairs-with-each (first l) (rest l))
                    (all-pairs (rest l)))])))
