#lang htdp/isl+
(require 2htdp/abstraction)

(define-struct add [left right])
(define-struct mul [left right])

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

; BSL-var-expr AL -> BSL-value
; evaluates ex with variables substituted by values from da
(check-expect (eval-variable* 'x al-1) 3)
(check-expect (eval-variable* (make-mul 'x 'y) al-2) 15)
(define (eval-variable* ex da)
  (local ((define subst-result
            (foldl (lambda (d expr) (subst expr (first d) (second d)))
                   ex
                   da)))
    (eval-variable subst-result)))

; BSL-var-expr -> BSL-value
; determines the value of ex if numeric? yields #true for it,
; otherwise signals an error
(define (eval-variable ex)
  (if (numeric? ex)
      (match ex
        [(? number?) ex]
        [(add left right) (+ (eval-expression left) (eval-expression right))]
        [(mul left right) (* (eval-expression left) (eval-expression right))])
      (error "Not a numeric expression")))

; BSL-var-expr Symbol Number -> BSL-var-expr
; produces a BSL-var-expr like ex with all
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
                                (subst right x v))]))

; BSL-expr -> BSL-value
; evaluates a BSL-expr
(define (eval-expression bsl-expr)
  (match bsl-expr
    [(? number?) bsl-expr]
    [(add left right) (+ (eval-expression left) (eval-expression right))]
    [(mul left right) (* (eval-expression left) (eval-expression right))]))
