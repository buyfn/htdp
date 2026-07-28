#lang htdp/isl+
(require 2htdp/abstraction)

(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
; BT (short for BinaryTree) is one of:
; - NONE
; - (make-node Number Symbol BT BT)

(define bt-1 (make-node
              15
              'd
              NONE
              (make-node
               24 'i NONE NONE)))
(define bt-2 (make-node
              42
              'x
              (make-node
               7 'y NONE NONE)
              NONE))

; Number BST -> Symbol or NONE
; If the tree contains ssn n produces the corresponding name
; otherwise produces NONE
(check-expect (search-bst 15 NONE) NONE)
(check-expect (search-bst 15 bt-1) 'd)
(check-expect (search-bst 24 bt-1) 'i)
(check-expect (search-bst 99 bt-1) NONE)
(check-expect (search-bst 42 bt-2) 'x)
(check-expect (search-bst 7 bt-2) 'y)
(check-expect (search-bst 99 bt-2) NONE)
(define (search-bst n bst)
  (match bst
    [(no-info) NONE]
    [(node ssn name left right)
     (cond
       [(= ssn n) name]
       [(< ssn n) (search-bst n right)]
       [else (search-bst n left)])]))
