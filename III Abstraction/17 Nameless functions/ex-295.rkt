#lang htdp/isl+

; distances in terms of pixels
(define WIDTH 300)
(define HEIGHT 300)

; Posn -> Boolean
; determines whether Posn p is within
; WIDTH and HEIGHT
(define (inside-playground? p)
  (and (<= 0 (posn-x p))
       (< (posn-x p) WIDTH)
       (<= 0 (posn-y p))
       (< (posn-y p) HEIGHT)))

; N -> [[List-of Posn] -> Boolean]
; a specification for `random-posns`
(define (n-inside-playground? n)
  (lambda (posns)
    (and (= n (length posns))
         (andmap inside-playground? posns))))

; N -> [List-of Posn]
; generates n random Posns in [0, Width) by [0, HEIGHT)
(check-satisfied (random-posns 3)
                 (n-inside-playground? 3))
(define (random-posns n)
  (build-list
   n
   (lambda (i)
     (make-posn (random WIDTH) (random HEIGHT)))))

; N -> [List-of Posn]
; generates n Posns (make-posn 0 0)
(check-satisfied (random-posns/bad 3)
                 (n-inside-playground? 3))
(define (random-posns/bad n)
  (build-list n
              (lambda (i)
                (make-posn 0 0))))
