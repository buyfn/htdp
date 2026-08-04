#lang htdp/isl+
(require 2htdp/abstraction)

(define-struct add [left right])
(define-struct mul [left right])

; A BSL-var-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-var-expr)
; - (make-mul BSL-var-expr BSL-var-expr)

; BSL-var-expr -> Boolean
; determines whether a BSL-var-expr ex is also a BSL-expr
(check-expect (numeric? 3) #true)
(check-expect (numeric? 'x) #false)
(check-expect (numeric? (make-add 1 2)) #true)
(check-expect (numeric? (make-mul 2 'y)) #false)
(define (numeric? ex)
  (match ex
    [(? number?) #true]
    [(? symbol?) #false]
    [(add left right) (and (numeric? left) (numeric? right))]
    [(mul left right) (and (numeric? left) (numeric? right))]))
