#lang htdp/isl+
(require 2htdp/abstraction)

(define-struct add [left right])
(define-struct mul [left right])

; A BSL-value is a Number

; A BSL-var-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-var-expr)
; - (make-mul BSL-var-expr BSL-var-expr)

; An AL (short for association list) is [List-of Association]
; An Association is a list of two items:
; (cons Symbol (cons Number '()))
(define al-1 (list (list 'x 3)))
(define al-2 (list (list 'x 3) (list 'y 5)))

; BSL-var-expr AL -> BSL-value
; evaluates ex using da to lookup variable values
(check-expect (eval-var-lookup 'x al-1) 3)
(check-expect (eval-var-lookup (make-mul 'x 'y) al-2) 15)
(define (eval-var-lookup ex da)
  (match ex
    [(? number?) ex]
    [(? symbol?) (local ((define d (assq ex da)))
                   (if (false? d)
                       (error "Undefined variable error")
                       (second d)))]
    [(add left right) (+ (eval-var-lookup left da)
                         (eval-var-lookup right da))]
    [(mul left right) (* (eval-var-lookup left da)
                         (eval-var-lookup right da))]))
