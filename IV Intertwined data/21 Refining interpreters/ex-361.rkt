#lang htdp/isl+
(require 2htdp/abstraction)

(define UNDEFINED_FUN_ERROR "Function is not defined")
(define UNDEFINED_CONST_ERROR "Constant is not defined")

(define-struct fun-def [name arg body])
(define-struct fun-application [name arg])
(define-struct add [left right])
(define-struct mul [left right])

; A BSL-value is a Number

; A BSL-fun-def is a structure
; (make-fun-def Symbol Symbol BSL-fun-expr)

; A BSL-da-all is a [List-of Definition]
; interpretation: represents constant and function definitions
; inside definitions area

; A Definition is one of:
; - Association
; - BSL-fun-def

; An Association is a list of two items
; (cons Symbol (cons Number '()))

(define da-all
  (list (list 'close-to-pi 3.14)
        (make-fun-def 'area-of-circle 'r
                      (make-mul 'close-to-pi
                                (make-mul 'r 'r)))
        (make-fun-def 'volume-of-10-cylinder 'r
                      (make-mul 10
                                (make-fun-application
                                 'area-of-circle 'r)))))

; A BSL-fun-expr is one of:
; - Number
; - Symbol
; - (make-fun-application Symbol BSL-fun-expr)
; - (make-add BSL-fun-expr BSL-fun-expr)
; - (make-mul BSL-fun-expr BSL-fun-expr)

; BSL-fun-expr BSL-da-all -> BSL-value
; evaluates expression ex
(check-expect (eval-all 'close-to-pi da-all) 3.14)
(check-expect (eval-all (make-add 'close-to-pi 3) da-all) 6.14)
(check-expect (eval-all (make-fun-application 'area-of-circle 1) da-all) 3.14)
(check-expect (eval-all
               (make-fun-application 'volume-of-10-cylinder
                                     (make-mul 1 1))
               da-all)
              31.4)
(define (eval-all ex da)
  (match ex
    [(? number?) ex]
    [(? symbol?) (second (lookup-con-def da ex))]
    [(add left right) (+ (eval-all left da)
                         (eval-all right da))]
    [(mul left right) (* (eval-all left da)
                         (eval-all right da))]
    [(fun-application name arg)
     (local ((define arg-value (eval-all arg da))
             (define fun (lookup-fun-def da name))
             (define plugd (subst (fun-def-body fun)
                                  (fun-def-arg fun)
                                  arg-value)))
       (eval-all plugd da))]))

; BSL-da-all Symbol -> BSL-fun-def
; produces a representation of function definition f, if exists in da
; otherwise signals an error
(check-expect (lookup-fun-def da-all 'area-of-circle)
              (make-fun-def 'area-of-circle
                            'r
                            (make-mul 'close-to-pi (make-mul 'r 'r))))
(check-error (lookup-fun-def da-all 'fact) UNDEFINED_FUN_ERROR)
(define (lookup-fun-def da f)
  (match da
    [(? empty?) (error UNDEFINED_FUN_ERROR)]
    [(cons (fun-def name arg body) tail)
     (if (symbol=? name f)
         (make-fun-def name arg body)
         (lookup-fun-def tail f))]
    [(cons head tail) (lookup-fun-def tail f)]))

; BSL-da-all Symbol -> Association
; produces representation of constant definition x, if exists in da;
; otherwise signals an error
(check-expect (lookup-con-def da-all 'close-to-pi)
              (list 'close-to-pi 3.14))
(check-error (lookup-con-def da-all 'unknown) UNDEFINED_CONST_ERROR)
(define (lookup-con-def da x)
  (match da
    [(? empty?) (error UNDEFINED_CONST_ERROR)]
    [(cons (fun-def name arg body) tail) (lookup-con-def tail x)]
    [(cons head tail) (if (symbol=? (first head) x)
                          head
                          (lookup-con-def tail x))]))

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
