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

; S-expr Symbol Atom -> S-expr
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
(define (substitute sexp old new)
  (cond
    [(atom? sexp) (if (equal? sexp old) new sexp)]
    [else
     (map (lambda (s) (substitute s old new)) sexp)]))

; We had to use lambda because map requires a funciton of one argument,
; so we couldn't supply `substitute` itself, since it's a function of 3 arguments
