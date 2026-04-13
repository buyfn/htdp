;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-58) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define LOW_PRICE 1000)
(define LUXURY_PRICE 10000)
(define NORMAL_TAX_RATE 0.05)
(define LUXURY_TAX_RATE 0.08)

; A Price falls into one of three intervals:
; - [0, 1000)
; - [1000, 10000)
; - 10000 and above
; interpretation the price of an item

; Price -> Number
; computes the amount of tax charged for p
(check-expect (sales-tax 0) 0)
(check-expect (sales-tax 537) 0)
(check-expect (sales-tax 1000) (* 0.05 1000))
(check-expect (sales-tax 1282) (* 0.05 1282))
(check-expect (sales-tax 10000) (* 0.08 10000))
(check-expect (sales-tax 12017) (* 0.08 12017))
(define (sales-tax p)
  (cond
    [(and (<= 0 p) (< p LOW_PRICE)) 0]
    [(and (<= LOW_PRICE p) (< p LUXURY_PRICE)) (* NORMAL_TAX_RATE p)]
    [(>= p LUXURY_PRICE) (* LUXURY_TAX_RATE p)]))