#lang htdp/isl+
(require 2htdp/abstraction)

(define UNDEFINED_FUN_ERROR "Function is not defined")
(define NOT_NUMERIC_ERROR "Not a numeric expression")

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
(check-error (lookup-def da-fgh 'i) UNDEFINED_FUN_ERROR)
(define (lookup-def da f)
  (match da
    [(? empty?) (error UNDEFINED_FUN_ERROR)]
    [(cons head tail) (if (symbol=? (fun-def-name head) f)
                          head
                          (lookup-def tail f))]))

; BSL-fun-expr BSL-fun-def* -> BSL-value
; evaluates ex, assuming definitions area contains da
(check-expect (eval-function* (make-fun-application 'f 10) da-fgh) 13)
(check-expect (eval-function* (make-fun-application 'g 1) da-fgh) 5)
(check-expect (eval-function* (make-fun-application 'h 1) da-fgh) 9)
(check-error (eval-function* (make-fun-application 'i 0) da-fgh)
             UNDEFINED_FUN_ERROR)
(define (eval-function* ex da)
  (cond
    [(not (numeric? ex)) (error NOT_NUMERIC_ERROR)]
    [(number? ex) ex]
    [(add? ex) (+ (eval-function* (add-left ex) da)
                  (eval-function* (add-right ex) da))]
    [(mul? ex) (* (eval-function* (mul-left ex) da)
                  (eval-function* (mul-right ex) da))]
    [(fun-application? ex)
     (local ((define arg-value (eval-function* (fun-application-arg ex) da))
             (define fun (lookup-def da (fun-application-name ex)))
             (define plugd (subst (fun-def-body fun)
                                  (fun-def-arg fun)
                                  arg-value)))
       (eval-function* plugd da))]))

; BSL-fun-expr -> Boolean
; determines whether a BSL-fun-expr ex contains no variable expressions
(check-expect (numeric? 3) #true)
(check-expect (numeric? 'x) #false)
(check-expect (numeric? (make-add 1 2)) #true)
(check-expect (numeric? (make-mul 2 'y)) #false)
(define (numeric? ex)
  (match ex
    [(? number?) #true]
    [(? symbol?) #false]
    [(add left right) (and (numeric? left) (numeric? right))]
    [(mul left right) (and (numeric? left) (numeric? right))]
    [(fun-application name arg) (numeric? arg)]))

; BSL-fun-expr Symbol Number -> BSL-fun-expr
; produces a BSL-fun-expr like ex with all
; occurrences of x replaced with v
(check-expect (subst 3 'x 5) 3)
(check-expect (subst 'x 'x 5) 5)
(check-expect (subst 'y 'x 5) 'y)
(check-expect (subst (make-add 'x 3) 'x 5) (make-add 5 3))
(check-expect (subst (make-mul (make-add 'x 'y) 'x) 'x 5)
              (make-mul (make-add 5 'y) 5))
(define (subst ex x v)
  (match ex
    [(? number?) ex]
    [(? symbol?) (if (symbol=? ex x) v ex)]
    [(add left right) (make-add (subst left x v)
                                (subst right x v))]
    [(mul left right) (make-mul (subst left x v)
                                (subst right x v))]
    [(fun-application name arg)
     (make-fun-application name (subst arg x v))]))
