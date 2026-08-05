#lang htdp/isl+
(require 2htdp/abstraction)

(define-struct f-application [name arg])
(define-struct add [left right])
(define-struct mul [left right])

; A BSL-value is a Number

; A BSL-fun-expr is one of:
; - Number
; - Symbol
; - (make-f-application Symbol BSL-fun-expr)
; - (make-add BSL-fun-expr BSL-fun-expr)
; - (make-mul BSL-fun-expr BSL-fun-expr)

(make-f-application 'k (make-add 1 1))
(make-mul 5 (make-f-application 'k (make-add 1 1 )))
(make-mul (make-f-application 'i 5)
          (make-f-application 'k (make-add 1 1)))

; BSL-fun-expr Symbol Symbol BSL-fun-expr -> BSL-value
; determines the value of ex
(check-expect (eval-definition1 1 'f 'x (make-add 'x 'x)) 1)
(check-expect (eval-definition1 (make-f-application 'f 2) 'f 'x (make-add 'x 'x)) 4)
(check-expect (eval-definition1 (make-f-application 'f (make-add 1 2))
                                'f 'x (make-mul 'x 'x))
              9)
(check-error (eval-definition1 'y 'f 'x 'x) "Not a numeric expression")
(check-error (eval-definition1 (make-f-application 'g 2)
                               'f 'x (make-add 'x 'x))
             "Unknown function name in application")
(define (eval-definition1 ex f x b)
  (if (numeric? ex)
      (match ex
        [(? number?) ex]
        [(add left right) (+ (eval-definition1 left f x b)
                             (eval-definition1 right f x b))]
        [(mul left right) (* (eval-definition1 left f x b)
                             (eval-definition1 right f x b))]
        [(f-application name arg)
         (if (symbol=? name f)
             (local ((define value (eval-definition1 arg f x b))
                     (define plugd (subst b x value)))
               (eval-definition1 plugd f x b))
             (error "Unknown function name in application"))])
      (error "Not a numeric expression")))

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
    [(f-application name arg)
     (make-f-application name (subst arg x v))]))

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
    [(f-application name arg) (numeric? arg)]))

;; Example of a non-terminating expression
;; (eval-definition1 (make-f-application 'f 0) 'f 'x (make-f-application 'f 'x))
