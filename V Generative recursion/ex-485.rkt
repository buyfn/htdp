#lang htdp/isl+

; A NumberTree is one of:
; - Number
; - (list NumberTree NumberTree)

; Abstract running time: n
; Acceptable measure of a size of a tree: number of nodes
; The shape doesn't matter, each node is visited once

(define nt-0 1)
(define nt-1
  (list (list 1 2) 3))
(define nt-2
  (list (list 4 (list 5 6)) nt-1))

; NumberTree -> Number
; determines the sum of the numbers in a tree
(check-expect (sum-tree nt-0) 1)
(check-expect (sum-tree nt-1) 6)
(check-expect (sum-tree nt-2) 21)
(define (sum-tree t)
  (cond
    [(number? t) t]
    [else (+ (sum-tree (first t))
             (sum-tree (second t)))]))
