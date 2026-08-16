#lang htdp/isl+

;; An XMachine is a nested list of this shape:
;; (cons 'machine (cons (list (list 'initial FSM-State)) [List-of X1T]))
;; An X1T is a nested list of this shape:
;; (list 'action (list (list 'state FSM-State)
;;                     (list 'next FSM-State)))

;; An XMachine is a nested list of this shape:
;; (cons 'machine
;;   (cons (cons (cons 'initial (cons FSM-State '()) '())
;;   [List-of X1T]))
;; An X1T is a nested list of this shape:
;; (cons 'action
;;       (cons (cons (cons 'state (cons FSM-State '()))
;;                   (cons (cons 'next (cons FSM-State '()))
;;                         '()))
;;             '()))
