;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex-245) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

; Number -> Number
; always returns 0
(define (zero x) 0)

; [Number -> Number] [Number -> Number] -> Boolean
; Given two functions f and g from numbers to numbers,
; determines whether they produce the same results for
; 1.2, 3 and -5.775
(check-expect (function=at-1.2-3-and-5.775? add1 zero) #false)
(check-expect (function=at-1.2-3-and-5.775? add1 add1) #true)
(define (function=at-1.2-3-and-5.775? f g)
  (and (= (f 1.2) (g 1.2))
       (= (f 3) (g 3))
       (= (f -5.775) (g -5.775))))
