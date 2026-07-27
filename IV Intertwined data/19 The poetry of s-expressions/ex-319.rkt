#lang htdp/isl+
(require 2htdp/abstraction)

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

; S-expr Symbol Symbol -> S-expr
; replaces instances of `old` in `s` with `new`
(check-expect (substitute 'a 'a 'b) 'b)
(check-expect (substitute 'a 'x 'b) 'a)
(check-expect (substitute 1 'a 'b) 1)
(check-expect (substitute "a" 'a 'b) "a")
(check-expect (substitute '() 'a 'b) '())
(check-expect (substitute (list 'a 'b 'a) 'a 'z) (list 'z 'b 'z))
(check-expect (substitute (list 'c (list 'd)) 'a 'z) (list 'c (list 'd)))
(check-expect (substitute (list 'a (list 'a 'b) 'c) 'a 'z)
              (list 'z (list 'z 'b) 'c))
(define (substitute s old new)
  (local (; Atom -> Atom
          ; if atom is a symbol `old`, replace with `new`
          (define (substitute-atom a)
            (if (equal? a old) new a))
          ; SL -> SL
          ; replaces instances of `old` in sl with `new`
          (define (substitute-sl sl)
            (match sl
              ['() '()]
              [(cons head tail) (cons (substitute-sexp head)
                                      (substitute-sl tail))]))
          ; S-expr -> S-expr
          ; replaces instances of `old` in sexp with `new`
          (define (substitute-sexp sexp)
            (cond
              [(atom? sexp) (substitute-atom sexp)]
              [else (substitute-sl sexp)])))
    (substitute-sexp s)))
