;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex-263) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define l1 '(25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
(define l2 (reverse l1))

; [Number Number -> Boolean] Nelon -> Number
; produces the item from l that beats every other item
; according to comparison beats?
(define (extremum beats? l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (local ((define extremum-in-rest
                    (extremum beats? (rest l))))
            (if (beats? (first l) extremum-in-rest)
                (first l)
                extremum-in-rest))]))

; Nelon -> Number
; determines the smallest number on l
(check-expect (inf-1 l1) 1)
(check-expect (inf-1 l2) 1)
(define (inf-1 l) (extremum < l))

; Nelon -> Number
; determines the largest number on l
(check-expect (sup-1 l1) 25)
(check-expect (sup-1 l2) 25)
(define (sup-1 l) (extremum > l))

; (inf-1 (list 2 1 3))
(sup-1 (list 2 1 3))