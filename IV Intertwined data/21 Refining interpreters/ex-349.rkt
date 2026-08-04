#lang htdp/isl+
(require 2htdp/abstraction)

(define-struct add [left right])
(define-struct mul [left right])

; A BSL-expr is one of:
; - Number
; - (make-add BSL-expr BSL-expr)
; - (make-mul BSL-expr BSL-expr)
; interpretation: represents a valid BSL expression

(define WRONG "could not parse invalid BSL expression")

; S-expr -> BSL-expr
(check-expect (parse '(+ 1 2)) (make-add 1 2))
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))

; SL -> BSL-expr
(check-expect (parse-sl '(+ 1 2)) (make-add 1 2))
(check-error (parse-sl '(+ 1)) WRONG)
(check-expect (parse-sl '(* 2 2)) (make-mul 2 2))
(check-error (parse-sl '(+ 1 2 3)) WRONG)
(check-error (parse-sl '(= 1 2)) WRONG)
(define (parse-sl s)
  (local ((define L (length s)))
    (cond
      [(< L 3) (error WRONG)]
      [(and (= L 3) (symbol? (first s)))
       (cond
         [(symbol=? (first s) '+)
          (make-add (parse (second s)) (parse (third s)))]
         [(symbol=? (first s) '*)
          (make-mul (parse (second s)) (parse (third s)))]
         [else (error WRONG)])]
      [else (error WRONG)])))

; Atom -> BSL-expr
(check-expect (parse-atom 1) 1)
(check-error (parse-atom "string") WRONG)
(check-error (parse-atom '+) WRONG)
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))

; Any -> Boolean
; determines whether passed value is an atom
(check-expect (atom? 1) #true)
(check-expect (atom? "hi") #true)
(check-expect (atom? 'hi) #true)
(check-expect (atom? (list 1 2)) #false)
(check-expect (atom? identity) #false)
(define (atom? v)
  (or (number? v)
      (string? v)
      (symbol? v)))

; S-expr -> BSL-value
; evaluates S-expr if parse recognises it a a valid BSL-expr
(check-expect (interpreter-expr '(+ 1 2)) 3)
(check-expect (interpreter-expr '(* 2 3)) 6)
(check-error (interpreter-expr '(/ 4 2)) WRONG)
(define (interpreter-expr sexp)
  (eval-expression (parse sexp)))

; BSL-expr -> BSL-value
; evaluates a BSL-expr
(define (eval-expression bsl-expr)
  (match bsl-expr
    [(? number?) bsl-expr]
    [(add left right) (+ (eval-expression left) (eval-expression right))]
    [(mul left right) (* (eval-expression left) (eval-expression right))]))
