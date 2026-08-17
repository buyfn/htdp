#lang htdp/isl+

; An XMachine is a nested list of this shape:
;   (cons 'machine (cons `((initial ,FSM-State))  [List-of X1T]))
; An X1T is a nested list of this shape:
;   `(action ((state ,FSM-State) (next ,FSM-State)))

;; <machine initial="white">
;;   <action state="white" next="black" />
;;   <action state="black" next="white" />
;; </machine>

(define wb-machine
  '(machine ((initial "white"))
            (action ((state "white") (next "black")))
            (action ((state "black") (next "white")))))
