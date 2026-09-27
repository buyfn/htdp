#lang htdp/isl+

(define ε 0.01)

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R, (+ R ε)]
; assume f is continuous and monotonically increasing
; assume (<= (f l) 0 (f r))
; generative divides interval in half, the root is in one of the two
; halves, picks according to assumption
; termination since the search interval is halved each step,
; the function terminates after n steps when n >= log(2)S1 - log(2)ε,
; where S1 is the initial interval width
(check-satisfied (find-root f -3 0)
                 (lambda (x) (<= (f x) 0 (f (+ x ε)))))
(define (find-root f l r)
  (if (<= (- r l) ε)
      l
      (local ((define mid (/ (+ l r) 2))
              (define f@mid (f mid)))
        (cond
          [(<= 0 f@mid) (find-root f l mid)]
          [else
           (find-root f mid r)]))))

; Number -> Number
(define (f x)
  (+ 4 (* x 2)))
