#lang htdp/isl+
(require 2htdp/abstraction)

(define-struct no-parent [])
(define-struct child [father mother name date eyes])
(define NP (make-no-parent))
; An FT (short for family tree) is one of:
; - NP
; - (make-child FT FT String N String)

; Oldest generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))

; Middle generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))

; Youngest generation:
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; FT -> N
; counts the child structures in the family tree
(check-expect (count-persons NP) 0)
(check-expect (count-persons Carl) 1)
(check-expect (count-persons Adam) 3)
(check-expect (count-persons Gustav) 5)
(define (count-persons a-tree)
  (match a-tree
    [(no-parent) 0]
    [(child father mother name date eyes)
     (+ 1 (count-persons father) (count-persons mother))]))
