#lang htdp/isl+

(define EUR/USD 1.06)

; [List-of Number] -> [List-of Number]
; converts a list of USD to a list of EUR
(check-expect (convert-euro '()) '())
(check-expect (convert-euro (list 1.06)) (list 1))
(check-expect (convert-euro (list 1.06 2.12))
              (list 1 2))
(define (convert-euro lod)
  (map (lambda (usd-amount) (/ usd-amount EUR/USD)) lod))

; [List-of Number] -> [List-of Number]
; converts a list of Fahrenheit measurements to Celsius
(check-expect (convertFC '()) '())
(check-expect (convertFC (list 32)) (list 0))
(check-expect (convertFC (list 212)) (list 100))
(define (convertFC lof)
  (map (lambda (t) (* (- t 32) 5/9)) lof))

; A Pair is a list of two numbers

; [List-of Posn] -> [List-of Pair]
; translates a list of Posns to
; a list of lists of pairs of numbers, where
; (make-posn x y) -> (list x y)
(check-expect (translate '()) '())
(check-expect (translate (list (make-posn 1 2)))
              (list (list 1 2)))
(check-expect (translate (list (make-posn 1 2) (make-posn 3 4)))
              (list (list 1 2) (list 3 4)))
(define (translate posns)
  (map (lambda (p) (list (posn-x p) (posn-y p))) posns))
