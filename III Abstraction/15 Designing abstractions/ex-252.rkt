#lang htdp/isl
(require 2htdp/image)

; [X Y -> Y] Y [List-of X] -> Y
; aggregates all the values from l into
; a single value using f pairwise
(define (fold1 f initial-val l)
  (cond
    [(empty? l) initial-val]
    [else (f (first l)
             (fold1 f initial-val (rest l)))]))

; [List-of Number] -> Number
(check-expect (product '()) 1)
(check-expect (product '(2)) 2)
(check-expect (product '(2 3 4)) 24)
(define (product l) (fold1 * 1 l))
	
; [List-of Posn] -> Image
(define (image* l) (fold1 place-dot emt l))
 
; Posn Image -> Image 
(define (place-dot p img)
  (place-image
     dot
     (posn-x p) (posn-y p)
     img))
 
; graphical constants:    
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))
