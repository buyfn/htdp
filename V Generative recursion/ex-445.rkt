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
                 (lambda (x) (<= (* -1 ε) (poly x) ε)))
(define (find-root f left right)
  (cond
    [(<= (- right left) ε) left]
    [else
     (local ((define mid (/ (+ left right) 2))
             (define f@mid (f mid)))
       (cond
         [(or (<= (f left) 0 f@mid) (<= f@mid 0 (f left)))
          (find-root f left mid)]
         [(or (<= f@mid 0 (f right)) (<= (f right) 0 f@mid))
          (find-root f mid right)]))]))

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

#|
| step |     left |        f left |     right |      f right |        mid |         f mid |
| n=1  |        3 |            -1 |      6.00 |         8.00 |       4.50 |          1.25 |
| n=2  |        3 |            -1 |      4.50 |         1.25 |       3.75 |       -0.4375 |
| n=3  |     3.75 |       -0.4375 |      4.50 |         1.25 |      4.125 |      0.265625 |
| n=4  |     3.75 |       -0.4375 |     4.125 |     0.265625 |     3.9375 |   -0.12109375 |
| n=5  |   3.9375 |   -0.12109375 |     4.125 |     0.265625 |    4.03125 |  0.0634765625 |
| n=6  |   3.9375 |   -0.12109375 |   4.03125 | 0.0634765625 |   3.984375 | -0.0310058594 |
| n=7  | 3.984375 | -0.0310058594 |   4.03125 | 0.0634765625 |  4.0078125 |  0.0156860352 |
| n=8  | 3.984375 | -0.0310058594 | 4.0078125 | 0.0156860352 | 3.99609375 | -0.0077972412 |

|#
