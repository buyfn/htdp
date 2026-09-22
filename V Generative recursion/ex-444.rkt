#lang htdp/isl+
(require 2htdp/abstraction)

; N N -> N
; find gcd
(check-expect (my-gcd 6 25) 1)
(check-expect (my-gcd 18 24) 6)
(define (my-gcd S L)
  (largest-common (divisors S S)
                  (divisors S L)))

; N[>= 0] N[>= 0] -> [List-of N]
; computes the divisors of l smaller or equal to k
; consumes k because we don't have to find all the divisors of the
; larger number to compute the common largest divisor
(check-expect (divisors 3 6) '(3 2 1))
(define (divisors k l)
  (cond
    [(= 0 k) '()]
    [(= (remainder l k) 0) (cons k (divisors (- k 1) l))]
    [else (divisors (- k 1) l)]))

; [List-of N] [List-of N] -> N
; finds the largest common to both k and l
(define (largest-common k l)
  (foldl (lambda (n m) (max n m)) 0 (intersect k l)))

; Son Son -> Son
; produces a set that contains numbers occurring in both input sets
(check-expect (intersect '() '()) '())
(check-expect (intersect '(1 2) '()) '())
(check-expect (intersect '() '(1 2)) '())
(check-expect (intersect '(1 2) '(1 2)) '(1 2))
(check-expect (intersect '(1 2) '(3 4)) '())
(check-expect (intersect '(1 2) '(2 3)) '(2))
(define (intersect s1 s2)
  (cond
    [(or (empty? s1) (empty? s2)) '()]
    [(member (first s1) s2)
     (cons (first s1) (intersect (rest s1) s2))]
    [else
     (intersect (rest s1) s2)]))
