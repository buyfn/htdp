;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-154) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct layer [color doll])
; An RD (short for russian doll) is one of:
; - String
; - (make-layer String RD)

; RD -> Number
; how many dolls are part of an-rd

; RD -> String
; produces a string of colors of all dolls
(check-expect (colors "red") "red")
(check-expect (colors (make-layer "yellow" (make-layer "green" "red")))
              "yellow, green, red")
(define (colors an-rd)
  (cond
    [(string? an-rd) an-rd]
    [else (string-append (layer-color an-rd)
                         ", "
                         (colors (layer-doll an-rd)))]))

; RD -> string
; returns the color on the innermost doll
(check-expect (inner "red") "red")
(check-expect (inner (make-layer "yellow" (make-layer "green" "red")))
              "red")
(define (inner an-rd)
  (cond
    [(string? an-rd) an-rd]
    [else (inner (layer-doll an-rd))]))