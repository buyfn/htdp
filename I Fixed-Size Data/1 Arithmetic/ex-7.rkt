;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define sunny #true)
(define friday #false)

(or (not sunny) friday)

; 4 combinations of booleans are possible:
; sunny: true,  friday: true
; sunny: false, friday: false
; sunny: false, friday: true
; sunny: true,  friday: false
