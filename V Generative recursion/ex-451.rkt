#lang htdp/isl+

(define ε 0.01)

(define (close-to? n x)
  (<= (abs (- x n)) ε))

(define-struct table [length array])
; A Table is a structure:
; (make-table N [N -> Number])

; Table N -> Number
; looks up the ith value in array of t
(define (table-ref t i)
  ((table-array t) i))

(define table1 (make-table 3 (lambda (i) i)))
(define table2 (make-table 10 (lambda (i) (- i 2))))

; Table -> N
; Finds the smallest index for a root of the table.
; The root of a table t is a number in (table-array t) that is close to 0
(check-expect (find-linear table2) 2)
(define (find-linear t)
  (local ((define (iter i)
            (cond
              [(>= i (table-length t))
               (error "No root")]
              [(close-to? 0 (table-ref t i)) i]
              [else (iter (+ i 1))])))
    (iter 0)))

; Table -> N
; Finds the smallest index for a root of the table.
; The root of a table t is a number in (table-array t) that is close to 0
; assume root exists between 0 and (- (table-length t) 1)
; generative: divides interval in half, picks next half based on whether
; the value at middle is larger or smaller than 0
; termination: the interval gets smaller with each step,
; and since (- right left) is a natural number, it finally reaches the base case.
(check-expect (find-binary table2) 2)
(define (find-binary t)
  (local ((define (iter left right)
            (local ((define mid (quotient (+ right left) 2))
                    (define @mid (table-ref t mid))
                    (define @left (table-ref t left))
                    (define @right (table-ref t right)))
              (cond
                [(<= (- right left) 1) (if (close-to? 0 @left) left right)]
                [(close-to? 0 @mid) (iter left mid)]
                [(< @mid 0) (iter mid right)]
                [(> @mid 0) (iter left mid)]))))
    (iter 0 (- (table-length t) 1))))
