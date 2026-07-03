;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex-236) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

; Number Lon -> Lon
; adds n to each item on l
(define (plus-n n l)
  (cond
    [(empty? l) '()]
    [else (cons (+ n (first l))
                (plus-n n (rest l)))]))

; Lon -> Lon
; adds 1 to each item on l
(check-expect (add1* '()) '())
(check-expect (add1* '(1)) '(2))
(check-expect (add1* '(1 2)) '(2 3))
(define (add1* l) (plus-n 1 l))

; Lon -> Lon
; adds 5 to each item on l
(check-expect (plus5 '()) '())
(check-expect (plus5 '(1)) '(6))
(check-expect (plus5 '(1 2)) '(6 7))
(define (plus5 l) (plus-n 5 l))

; Lon -> Lon
; subtracts 2 from each item on l
(check-expect (minus2 '()) '())
(check-expect (minus2 '(1)) '(-1))
(check-expect (minus2 '(1 3)) '(-1 1))
(define (minus2 l) (plus-n -2 l))
