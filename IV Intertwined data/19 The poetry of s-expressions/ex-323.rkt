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

; Number BT -> [Maybe Symbol]
; If tree contains n, returns the value of the name field in that node,
; otherwise returns #false
(check-expect (search-bt 24 bt-1) 'i)
(check-expect (search-bt 24 bt-2) #false)
(check-expect (search-bt 15 bt-1) 'd)
(check-expect (search-bt 7 bt-2) 'y)
(define (search-bt n bt)
  (match bt
    [(no-info) #false]
    [(node ssn name left right)
     (if (= ssn n)
         name
         (local ((define left-result (search-bt n left)))
           (if (boolean? left-result)
               (search-bt n right)
               left-result)))]))
