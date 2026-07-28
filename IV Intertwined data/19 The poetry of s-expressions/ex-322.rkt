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

; BT Number -> Boolean
; determines whether a given number occurs in some given BT
(check-expect (contains-bt? NONE 1) #false)
(check-expect (contains-bt? bt-1 1) #false)
(check-expect (contains-bt? bt-1 15) #true)
(check-expect (contains-bt? bt-1 24) #true)
(check-expect (contains-bt? bt-2 7) #true)
(check-expect (contains-bt? bt-2 1) #false)
(define (contains-bt? bt n)
  (match bt
    [(no-info) #false]
    [(node ssn name left right)
     (or (= ssn n)
         (contains-bt? left n)
         (contains-bt? right n))]))
