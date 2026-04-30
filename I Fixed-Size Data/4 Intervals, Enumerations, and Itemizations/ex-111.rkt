;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-110) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

(define-struct vec [x y])
; A vec is
; (make-vec PositiveNumber PositiveNumber)
; interpretation represents a velocity vector

; Any Any -> vec
; (checked-make-vec Any Any)
; creates a vec if both arguments are positive numbers
(define (checked-make-vec x y)
  (cond
    [(and (and (number? x)
               (> x 0))
          (and (number? y)
               (> y 0)))
     (make-vec x y)]
    [else (error "checked-make-vec: positive numbers expected")]))
