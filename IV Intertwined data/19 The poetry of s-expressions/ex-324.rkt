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

; BT -> [List-of Number]
; produces the sequence of all the ssn numbers
; in the tree as they show up from left to right
(check-expect (inorder bt-1) (list 15 24))
(check-expect (inorder bt-2) (list 7 42))
(define (inorder bt)
  (match bt
    [(no-info) '()]
    [(node ssn name left right)
     (append (inorder left)
             (list ssn)
             (inorder right))]))

; inorder produces a sorted list of ssns for a binary search tree
