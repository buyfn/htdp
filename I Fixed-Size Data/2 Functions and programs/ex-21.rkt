;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-21) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (ff a) (* 10 a))

(ff (ff 1))
(+ (ff 1) (ff 1))

; Judging from how DrRacket steps through the second expression,
; it doesn't reuse the results of computations