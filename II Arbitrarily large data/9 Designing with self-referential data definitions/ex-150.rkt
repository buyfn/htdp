;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-150) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; N -> Number
; computes (+ n pi) without using +
(check-within (add-to-pi 3) (+ 3 pi) 0.001)
(define (add-to-pi n)
  (cond
    [(= n 0) pi]
    [else (add1 (add-to-pi (sub1 n)))]))

; Number, N -> Number
; copmutes (+ x n) without using +
(check-within (add 10 5) 15 0.001)
(define (add x n)
  (cond
    [(= n 0) x]
    [else (add (add1 x) (sub1 n))]))

; N, N -> Number
; computes (* x n) without using *
(check-expect (multiply 3 4) 12)
(define (multiply x n)
  (cond
    [(= n 1) x]
    [else (add x (multiply x (sub1 n)))]))
