#lang htdp/isl+
(require 2htdp/abstraction)

; A QP is a structure:
; (make-posn CI CI)
; A CI is an N in [0, n), where n is the size of a board
; interpretation: (make-posn r c) denotes the square at
; the r-th row and c-th column

; N -> [List-of QP]
; produces a list of all square on a board of size n
(define (all-squares n)
  (for*/list ((row n) (col n))
              (make-posn row col)))

; A Board is a structure
; [B] (make-board [N -> B] [B QP -> B] [B -> [List-of QP])
; interpretation: represents a Board structure with the operations:
; - make
; - add-queen
; - find-open-spots
(define-struct board [make add-queen find-open-spots])

; A Board-1 is a [List-of QP]
; interpretation: represents a list of all squares where
; a queen can be placed
(define board-1
  (local ((define (add-queen b qp)
            (filter (lambda (square)
                      (not (threatening? qp square))) b)))
    (make-board all-squares add-queen identity)))

; A Board-2 is a structure (make-board-2 N [List-of QP])
; interpretateion: (make-board-2 size queens) represents a board
; of given size and a list of position where a queen has been placed
(define board-2
  (local ((define-struct board-2 [size queens])
          (define (make n) (make-board-2 n '()))
          (define (add-queen b qp)
            (make-board-2 (board-2-size b)
                          (cons qp (board-2-queens b))))
          (define (find-open-spots b)
            (local (; QP -> Boolean
                    ; determines whether no queen on the board
                    ; threatens given square s
                    (define (safe-square? s)
                      (andmap (lambda (q) (not (threatening? q s)))
                              (board-2-queens b))))
              (filter safe-square? (all-squares (board-2-size b))))))
    (make-board make add-queen find-open-spots)))

; A Square is a structure (make-square CI CI Boolean)
; interpretation: (make-square x y threatened?) represents
; a square on a board in column y, row x and threatened? = #true if
; the square is threatened by a queen on a board
(define-struct square [x y threatened?])

; Square -> QP
; converts a Square to QP
(define (square->qp s)
  (make-posn (square-x s)
             (square-y s)))

; A Board-3 is a [List-of Square]
; interpretation: represents all squares on a given board
(define board-3
  (local ((define (make n)
            (map (lambda (s)
                   (make-square (posn-x s)
                                (posn-y s)
                                #false)) (all-squares n)))
          (define (add-queen b qp)
            (map (lambda (s)
                   (match s
                     [(square x y #false)
                      (if (threatening? qp (make-posn x y))
                          (make-square x y #true)
                          s)]
                     [(square x y #true) s]))
                 b))
          (define (find-open-spots b)
            (map square->qp
                 (filter (lambda (s) (false? (square-threatened? s)))
                         b))))
    (make-board make add-queen find-open-spots)))

; [B] Board B N -> [Maybe [List-of QP]]
; finds a solution to the n queens problem
; data example: [List-of QP]
(define 4QUEEN-SOLUTION-1
  (list (make-posn 0 1) (make-posn 1 3)
        (make-posn 2 0) (make-posn 3 2)))
(define 4QUEEN-SOLUTION-2
  (list (make-posn 0 2) (make-posn 1 0)
        (make-posn 2 3) (make-posn 3 1)))
(check-satisfied (n-queens board-1 4) (n-queens-solution? 4))
(check-satisfied (n-queens board-2 4) (n-queens-solution? 4))
(check-satisfied (n-queens board-3 4) (n-queens-solution? 4))
(define (n-queens board-type n)
  (place-queens board-type (board0 board-type n) n))

; [B] Board B N -> [Maybe [List-of QP]]
; places n queens on board; otherwise, returns #false
(define (place-queens board-type a-board n)
  (cond
    [(zero? n) '()]
    [else (place-queens/list board-type
                             (find-open-spots board-type a-board)
                             a-board
                             n)]))

; [List-of QP] Board N -> [Maybe [List-of QP]]
; places n queens on board by trying to place the first queen into
; one of the positions from open-spots; otherwise, returns #false
; Assume n is at least 1
; Termination: each recursive call to place-queens decreases n until it's zero;
; each recursive call to place-queens/list decreases the length of open-spots,
; until it's empty while n stays the same, so only finitely many calls happen
; before n decreases
(define (place-queens/list board-type open-spots a-board n)
  (cond
    [(empty? open-spots) #false]
    [else (local ((define candidate
                    (place-queens board-type
                                  (add-queen board-type a-board (first open-spots))
                                  (- n 1))))
            (cond
              [(boolean? candidate)
               (place-queens/list board-type (rest open-spots) a-board n)]
              [else (cons (first open-spots) candidate)]))]))

; Board N -> Board
; creates the initial n by n board
(define (board0 board-type n)
  ((board-make board-type) n))

; [B] Board B QP -> Board
; places a queen at qp on a-board
;; (define (add-queen a-board qp)
;;   (make-board (board-size a-board)
;;               (cons qp (board-queens a-board))))
(define (add-queen board-type b qp)
  ((board-add-queen board-type) b qp))

; [B] Board B -> [List-of QP]
; finds spots where it is still safe to place a queen
; Board-1
(check-expect (find-open-spots board-1 (board0 board-1 2))
              (list (make-posn 0 0) (make-posn 0 1)
                    (make-posn 1 0) (make-posn 1 1)))
(check-expect (find-open-spots board-1
                               (add-queen board-1
                                          (board0 board-1 2)
                                          (make-posn 0 0)))
              '())
(check-expect (find-open-spots board-1
                               (add-queen board-1
                                          (board0 board-1 3)
                                          (make-posn 0 0)))
              (list (make-posn 1 2) (make-posn 2 1)))
; Board-2
(check-expect (find-open-spots board-2 (board0 board-2 2))
              (list (make-posn 0 0) (make-posn 0 1)
                    (make-posn 1 0) (make-posn 1 1)))
(check-expect (find-open-spots board-2
                               (add-queen board-2
                                          (board0 board-2 2)
                                          (make-posn 0 0)))
              '())
(check-expect (find-open-spots board-2
                               (add-queen board-2
                                          (board0 board-2 3)
                                          (make-posn 0 0)))
              (list (make-posn 1 2) (make-posn 2 1)))
; Board-3
(check-expect (find-open-spots board-3 (board0 board-3 2))
              (list (make-posn 0 0) (make-posn 0 1)
                    (make-posn 1 0) (make-posn 1 1)))
(check-expect (find-open-spots board-3
                               (add-queen board-3
                                          (board0 board-3 2)
                                          (make-posn 0 0)))
              '())
(check-expect (find-open-spots board-3
                               (add-queen board-3
                                          (board0 board-3 3)
                                          (make-posn 0 0)))
              (list (make-posn 1 2) (make-posn 2 1)))
(define (find-open-spots board-type b)
  ((board-find-open-spots board-type) b))

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
