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

; [List-of String] -> [[List-of String] -> Boolean]
; creates a predicate that determines whether given list contains
; the same items as lst, not accounting for order
(define (permutation-of lst)
  (lambda (l) (equal? (sort lst string<?)
                      (sort l string<?))))

; FT -> [List-of String]
; produces a list of all eye colours in the tree
(check-expect (eye-colors NP) '())
(check-expect (eye-colors Carl) (list "green"))
(check-satisfied (eye-colors Eva)
                 (permutation-of (list "blue" "green" "green")))
(check-satisfied (eye-colors Gustav)
                 (permutation-of (list "brown" "blue" "pink" "green" "green")))
(define (eye-colors a-tree)
  (match a-tree
    [(no-parent) '()]
    [(child father mother name date eyes)
     (append (list eyes)
             (eye-colors father)
             (eye-colors mother))]))
