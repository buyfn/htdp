#lang htdp/isl+

(define FIELD-SIZE 20)

; Posn -> Posn
; generates a posn that is guaranteed to be different from
; the given one
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)
(define (food-create p)
  (local ((define (food-check-create candidate)
            (if (equal? p candidate)
                (food-create p)
                candidate)))
    (food-check-create
     (make-posn (random FIELD-SIZE) (random FIELD-SIZE)))))

; Posn -> Boolean
; use for testing only
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))
