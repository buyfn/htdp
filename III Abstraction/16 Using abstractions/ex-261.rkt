#lang htdp/isl

;; Execution time
;; (time (extract1 inv1)) => 
;; cpu time: 15 real time: 16 gc time: 0
;; (time (extract2 inv1)) => 
;; cpu time: 19 real time: 20 gc time: 4

; No measurable difference in performance.
; Expression (extract (rest an-inv)) is computed only
; once per function call because this expression
; is repeated in different clauses of `cond`, so using `local`
; doesn't help to improve performance

; An Inventory is one of:
; - '()
; - (cons IR Inventory)

(define-struct IR [name price])
; An IR is a structure:
; (make-IR String Number)
; interpretation an IR (inventory record) associates
; the name of an item with its price

; Number -> IR
(define (create-IR p) (make-IR "test name" p))

(define inv1 (build-list 100000 create-IR))

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost a dollar or less
(check-expect (extract1 inv1)
              (list (make-IR "test name" 0)
                    (make-IR "test name" 1)))
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (cond
       [(<= (IR-price (first an-inv)) 1.0)
        (cons (first an-inv) (extract1 (rest an-inv)))]
       [else (extract1 (rest an-inv))])]))

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost a dollar or less
(check-expect (extract2 inv1)
              (list (make-IR "test name" 0)
                    (make-IR "test name" 1)))
(define (extract2 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else (local ((define extract2-in-rest (extract2 (rest an-inv))))
            (cond
              [(<= (IR-price (first an-inv)) 1.0)
               (cons (first an-inv) extract2-in-rest)]
              [else extract2-in-rest]))]))
