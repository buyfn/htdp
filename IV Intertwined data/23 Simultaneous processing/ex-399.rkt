#lang htdp/isl+
(require 2htdp/abstraction)

(define NAMES '("Louise" "Jane" "Laura" "Dana" "Mary"))

; [List-of String] -> [List-of String] 
; picks a random non-identity arrangement of names
(define (gift-pick names)
  (random-pick
    (non-same names (arrangements names))))
 
; [List-of String] -> [List-of [List-of String]]
; returns all possible permutations of names
(define (arrangements names)
  (cond
    [(empty? names) '(())]
    [else (insert-everywhere/in-all-lists
           (first names)
           (arrangements (rest names)))]))

; String [List-of [List-of Strings]] -> [List-of [List-of String]]
(check-expect (insert-everywhere/in-all-lists "x" '(("one" "two")))
              '(("x" "one" "two")
                ("one" "x" "two")
                ("one" "two" "x")))
(define (insert-everywhere/in-all-lists s los)
  (cond
    [(empty? los) '()]
    [else (append (insert-everywhere/list s (first los))
                  (insert-everywhere/in-all-lists s (rest los)))]))

; String [List-of String] -> [List-of [List-of String]]
(define (insert-everywhere/list s l)
  (cond
    [(empty? l) (list (list s))]
    [else (cons (cons s l)
                (prepend-s/for-each-list (first l)
                                         (insert-everywhere/list s (rest l))))]))

; String [List-of [List-of String]] -> [List-of [List-of String]]
(define (prepend-s/for-each-list s l)
  (map (lambda (l) (cons s l)) l))

; [NEList-of X] -> X 
; returns a random item from the list 
(define (random-pick l)
  (local ((define random-pos (random (length l))))
    (list-ref l random-pos)))
 
; [List-of String] [List-of [List-of String]] 
; -> 
; [List-of [List-of String]]
; produces the list of those lists in ll that do 
; not agree with names at any place
(check-expect (non-same '("one" "two" "three")
                        '(("one" "two" "three")
                          ("one" "three" "two")
                          ("three" "one" "two")))
              '(("three" "one" "two")))
(define (non-same names ll)
  (local (; [List-of String] [List-of String] -> Boolean
          ; returns true if at any position elements
          ; match between lon-1 and lon-2
          ; otherwise returns false
          (define (same? lon-1 lon-2)
            (cond
              [(empty? lon-1) #false]
              [(string=? (first lon-1) (first lon-2)) #true]
              [else (same? (rest lon-1) (rest lon-2))])))
    (cond
      [(empty? ll) '()]
      [(same? names (first ll)) (non-same names (rest ll))]
      [else (cons (first ll)
                  (non-same names (rest ll)))])))
