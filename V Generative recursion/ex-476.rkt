#lang htdp/isl+

(define-struct transition [current key next])
(define-struct fsm [initial transitions final])
 
; An FSM is a structure:
;   (make-fsm FSM-State [List-of 1Transition] FSM-State)
; A 1Transition is a structure:
;   (make-transition FSM-State 1String FSM-State)
; An FSM-State is String.
 
; data example: see exercise 109
 
(define fsm-a-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

; FSM String -> Boolean
; produces true if the sequence of characters in s
; causes the machine to transition from initial state to final
(check-expect (fsm-match? fsm-a-bc*-d "abcd") #true)
(check-expect (fsm-match? fsm-a-bc*-d "acbd") #true)
(check-expect (fsm-match? fsm-a-bc*-d "ad") #true)
(check-expect (fsm-match? fsm-a-bc*-d "da") #false)
(check-expect (fsm-match? fsm-a-bc*-d "aa") #false)
(check-expect (fsm-match? fsm-a-bc*-d "d") #false)
(define (fsm-match? machine s)
  (local ((define final-state (fsm-final machine))
          ; String 1String -> [Maybe String]
          ; returns the next state of the fsm
          ; derived from current-state and input
          (define (next-state current-state input)
            (local ((define transitions-from-current
                      (filter (lambda (t)
                                (string=? current-state
                                          (transition-current t)))
                              (fsm-transitions machine)))
                    (define matched-transition
                      (filter (lambda (t)
                                (string=? input (transition-key t)))
                              transitions-from-current)))
              (if (cons? matched-transition)
                  (transition-next (first matched-transition))
                  #false)))
          ; [Maybe String] [List-of 1String] -> Boolean
          (define (aux current rest-of-input)
            (cond
              [(boolean? current) #false]
              [(and (string=? current final-state)
                    (empty? rest-of-input))
               #true]
              [(cons? rest-of-input)
               (aux (next-state current (first rest-of-input))
                    (rest rest-of-input))]
              [else #false])))
    (aux (fsm-initial machine) (explode s))))
