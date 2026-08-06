#lang htdp/isl+
(require 2htdp/abstraction)

(define-struct fun-def [name arg body])
(define-struct fun-application [name arg])
(define-struct add [left right])
(define-struct mul [left right])

; A BSL-value is a Number

; A BSL-fun-def is a structure
; (make-fun-def Symbol Symbol BSL-fun-expr)
(define f (make-fun-def 'f 'x (make-add 3 'x)))
(define g
  (make-fun-def 'g 'y (make-fun-application 'f (make-mul 2 'y))))
(define h
  (make-fun-def 'h 'v (make-add (make-fun-application 'f 'v)
                                (make-fun-application 'g 'v))))

; A BSL-fun-def* is a [List-of BSL-fun-def]
(define da-fgh (list f g h))

; A BSL-fun-expr is one of:
; - Number
; - Symbol
; - (make-fun-application Symbol BSL-fun-expr)
; - (make-add BSL-fun-expr BSL-fun-expr)
; - (make-mul BSL-fun-expr BSL-fun-expr)

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def da-fgh 'g) g)
(check-error (lookup-def da-fgh 'i) "Function is not defined")
(define (lookup-def da f)
  (match da
    [(? empty?) (error "Function is not defined")]
    [(cons head tail) (if (symbol=? (fun-def-name head) f)
                          head
                          (lookup-def tail f))]))
