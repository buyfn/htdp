#lang htdp/isl+
(require 2htdp/image)
(require 2htdp/universe)

; An FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
; (cons FSM-State (cons FSM-State '()))
; An FSM-State is a String that specifies a color

; data examples
(define fsm-traffic
  '(("red" "green")
    ("green" "yellow")
    ("yellow" "red")))

; FSM FSM-State -> FSM-State
; matches the keys pressed by a player with the given FSM
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
            [to-draw (lambda (current)
                       (overlay (text current 12 "black")
                                (square 100 "solid" current)))]
            [on-key (lambda (current key-event)
                      (find transitions current))]))

; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-expect (find fsm-traffic "yellow") "red")
(check-error (find fsm-traffic "blue") "not found")
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))
