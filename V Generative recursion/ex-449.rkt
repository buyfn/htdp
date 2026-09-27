#lang htdp/isl+

(define ε 0.01)

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R, (+ R ε)]
; assume f is continuous
; assume (or (<= (f left) 0 (f right))
;            (<= (f right) 0 (f left)))
; generative divides interval in half, the root is in one of the two
; halves, picks according to assumption
; termination since the search inteval is halved each step,
; the function terminates after n steps when n >= log(2)S1 - log(2)ε,
; where S1 is the initial interval width
(check-satisfied (find-root poly 3 6)
                 (lambda (x) (or (<= (poly x) 0 (poly (+ x ε)))
                                 (<= (poly (+ x ε)) 0 (poly x)))))
(check-satisfied (find-root poly 1 3)
                 (lambda (x) (or (<= (poly x) 0 (poly (+ x ε)))
                                 (<= (poly (+ x ε)) 0 (poly x)))))
(define (find-root f left right)
  (local ((define (find-root-iter l r f@l f@r)
            (if (<= (- r l) ε)
                l
                (local ((define mid (/ (+ l r) 2))
                        (define f@mid (f mid)))
                  (cond
                    [(or (<= f@l 0 f@mid) (<= f@mid 0 f@l))
                     (find-root-iter l mid f@l f@mid)]
                    [(or (<= f@mid 0 f@r) (<= f@r 0 f@mid))
                     (find-root-iter mid r f@mid f@r)])))))
    (find-root-iter left right (f left) (f right))))

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

