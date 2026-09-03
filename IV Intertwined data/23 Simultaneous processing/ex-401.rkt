#lang htdp/isl+

; An S-expr (S-expression) is one of:
; - Atom
; - [List-of S-expression]

; An Atom is one of:
; - Number
; - String
; - Symbol

; S-expr S-expr -> Boolean
; determines whether two S-expressions are equal
(check-expect (sexp=? 1 1) #true)
(check-expect (sexp=? 1 2) #false)
(check-expect (sexp=? "a" "a") #true)
(check-expect (sexp=? "a" "b") #false)
(check-expect (sexp=? 'a 'a) #true)
(check-expect (sexp=? 'a 'b) #false)
(check-expect (sexp=? 'a "a") #false)
(check-expect (sexp=? '() '()) #true)
(check-expect (sexp=? '(1) '()) #false)
(check-expect (sexp=? '(1) '(1)) #true)
(check-expect (sexp=? '(1 (a)) '(1 (a))) #true)
(check-expect (sexp=? '(1 (a)) '(1 (a (b)))) #false)
(define (sexp=? s1 s2)
  (cond
    [(and (atom? s1) (atom? s2)) (atom=? s1 s2)]
    [(and (list? s1) (list? s2)) (list=? s1 s2)]
    [else #false]))

; S-expr -> Boolean
; determines whether an S-expression is an Atom
(check-expect (atom? 1) #true)
(check-expect (atom? "b") #true)
(check-expect (atom? 'a) #true)
(check-expect (atom? '()) #false)
(check-expect (atom? '(1)) #false)
(define (atom? sexpr)
  (or (number? sexpr)
      (string? sexpr)
      (symbol? sexpr)))

; Atom Atom -> Boolean
; determines whether two Atoms have the same value
(check-expect (atom=? 1 1) #true)
(check-expect (atom=? 1 2) #false)
(check-expect (atom=? "a" "a") #true)
(check-expect (atom=? "a" "b") #false)
(check-expect (atom=? 'a 'a) #true)
(check-expect (atom=? 'b 'a) #false)
(check-expect (atom=? 1 'a) #false)
(define (atom=? a1 a2)
  (cond
    [(and (number? a1) (number? a2)) (= a1 a2)]
    [(and (string? a1) (string? a2)) (string=? a1 a2)]
    [(and (symbol? a1) (symbol? a2)) (symbol=? a1 a2)]
    [else #false]))

; S-expr S-expr -> Boolean
; determines whether two lists are equal
(check-expect (list=?'() '()) #true)
(check-expect (list=? '(1) '()) #false)
(check-expect (list=? '(1) '(1)) #true)
(check-expect (list=? '(1 (a)) '(1 (a))) #true)
(check-expect (list=? '(1 ()) '(1 ())) #true)
(check-expect (list=? '(1 (a)) '(1 (a (b)))) #false)
(define (list=? l1 l2)
  (cond
    [(and (empty? l1) (empty? l2)) #true]
    [(and (cons? l1) (cons? l2))
     (and (sexp=? (first l1) (first l2))
          (sexp=? (rest l1) (rest l2)))]
    [else #false]))
