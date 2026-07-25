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

; [List-of Number] -> Number
; produces the average of numbers in the list
(check-expect (average (list 1 2 3)) 2)
(define (average lon)
  (/ (for/sum ([n lon]) n)
     (length lon)))

; FT N -> Number
; produces the average age of all child structures in the family tree
(check-expect (average-age Adam 2026) 92)
(check-expect (average-age Carl 1930) 4)
(check-expect (average-age Gustav 2026) 71.8)
(define (average-age a-tree current-year)
  (local (; FT -> [List-of N]
          ; produces a list of birth dates for every child in a tree
          (define (FT->date-list tree)
            (match tree
              [(no-parent) '()]
              [(child father mother name date eyes)
               (append (list date)
                       (FT->date-list father)
                       (FT->date-list mother))])))
    (average (map (lambda (date) (- current-year date))
                  (FT->date-list a-tree)))))
