#lang htdp/isl+
(require 2htdp/abstraction)

(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
; BT (short for BinaryTree) is one of:
; - NONE
; - (make-node Number Symbol BT BT)

;      10 'h
;     /     \
;  5 'e    24 'i
(define BST-5 (make-node 5 'e NONE NONE))
(define BST-24 (make-node 24 'i NONE NONE))
(define BST-10 (make-node 10 'h BST-5 BST-24))

(define bst-a-10 (make-node 10 'a NONE NONE))
(define bst-a-24 (make-node 24 'c NONE NONE))
(define bst-a-15 (make-node 15 'b bst-a-10 bst-a-24))
(define bst-a-29 (make-node 29 'd bst-a-15 NONE))

(define bst-a-99 (make-node 99 'i NONE NONE))
(define bst-a-95 (make-node 95 'h NONE bst-a-99))
(define bst-a-77 (make-node 77 'f NONE NONE))
(define bst-a-89 (make-node 89 'g bst-a-77 bst-a-95))

(define bst-a (make-node 63 'e bst-a-29 bst-a-89))

; BST Number Symbol -> BST
; inserts (make-node n s NONE NONE) into b
(check-expect (create-bst NONE 10 'h) (make-node 10 'h NONE NONE))
(check-expect (create-bst BST-10 3 'd)
              (make-node 10 'h
                         (make-node 5 'e (make-node 3 'd NONE NONE) NONE)
                         BST-24))
(check-expect (create-bst BST-10 7 'g)
              (make-node 10 'h
                         (make-node 5 'e NONE (make-node 7 'g NONE NONE))
                         BST-24))
(check-expect (create-bst BST-10 99 'o)
              (make-node 10 'h
                         BST-5
                         (make-node 24 'i NONE (make-node 99 'o NONE NONE))))
(check-expect (create-bst bst-a 69 'nice)
              (make-node 63 'e
                         bst-a-29
                         (make-node 89 'g
                                    (make-node 77 'f (make-node 69 'nice NONE NONE) NONE)
                                    bst-a-95)))
(define (create-bst b n s)
  (match b
    [(no-info) (make-node n s NONE NONE)]
    [(node ssn name left right)
     (if (< n ssn)
         (make-node ssn name (create-bst left n s) right)
         (make-node ssn name left (create-bst right n s)))]))
