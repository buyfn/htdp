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

; S-expr -> N
; determines depth of sexp
(check-expect (depth 'world) 1)
(check-expect (depth '(world hello)) 2)
(check-expect (depth '(((world) hello) hello)) 4)
(check-expect (depth '(hello (world (x)))) 4)
(check-expect (depth '()) 1)
(define (depth sexp)
  (local (; SL -> N
          ; determines max depth of items in sl
          (define (depth-sl sl)
            (match sl
              ['() 0]
              [(cons head tail) (max (depth head) (depth-sl tail))])))
    (cond
      [(atom? sexp) 1]
      [else (+ 1 (depth-sl sexp))])))
