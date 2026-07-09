#lang htdp/isl+

; [List-of Number] -> [List-of Number]
; converts a list of USD to a list of EUR
(check-expect (convert-euro '()) '())
(check-expect (convert-euro (list 1.06)) (list 1))
(check-expect (convert-euro (list 1.06 2.12))
              (list 1 2))
(define (convert-euro lod)
  (local (; 1 EUR costs 1.06 USD
          (define EUR/USD 1.06)
          ; Number -> Number
          ; converts USD amount to EUR
          (define (usd->eur usd-amount)
            (/ usd-amount EUR/USD)))
    (map usd->eur lod)))

; [List-of Number] -> [List-of Number]
; converts a list of Fahrenheit measurements to Celsius
(check-expect (convertFC '()) '())
(check-expect (convertFC (list 32)) (list 0))
(check-expect (convertFC (list 212)) (list 100))
(define (convertFC lof)
  (local (; Number -> Number
          ; converts a Fahrenheit measurement
          ; to Celsius
          (define (F->C t)
            (* (- t 32) 5/9)))
    (map F->C lof)))

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
  (local (; Posn -> [List-of Number]
          ; translates Posn to a list of pair of numbers
          (define (posn->pair p)
            (list (posn-x p) (posn-y p))))
    (map posn->pair posns)))
