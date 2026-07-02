;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-229) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))

; An FSM is one of:
; - '()
; - (cons Transition FSM)

(define-struct transition [current key next])
; A Transition is a structure:
; (make-transition FSM-State KeyEvent FSM-State)

; FSM-State is a Color

; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another
; in reaction to keystrokes

(define-struct fs [fsm current])
; A SimulationState.v2 is a structure:
; (make-fs FSM FSM-State)

; FSM-State FSM-State -> Boolean
; determines whether two states are equal
(check-expect (state=? "blue" "green") #false)
(check-expect (state=? "green" "green") #true)
(define (state=? s1 s2)
  (cond
    [(and (image-color? s1)
          (image-color? s2))
     (equal? s1 s2)]
    [else (error "state=?: at least one of the arguments is not a Color")]))

(define abcd-machine
  (list (make-transition "white" "a" "yellow")
        (make-transition "yellow" "b" "yellow")
        (make-transition "yellow" "c" "yellow")
        (make-transition "yellow" "d" "green")))

; FSM FSM-State -> SimulationState.v2
; match the keys pressed with the given FSM
(define (simulate.v2 an-fsm s0)
  (big-bang (make-fs an-fsm s0)
            [to-draw state-as-colored-square]
            [on-key find-next-state]))

; SimulationState.v2 -> Image
; renders a world state as an image
(check-expect (state-as-colored-square
               (make-fs abcd-machine "green"))
              (square 100 "solid" "green"))
(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current an-fsm)))

; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from ke and fsm
(check-expect
 (find-next-state (make-fs abcd-machine "white") "a")
 (make-fs abcd-machine "yellow"))
(check-error
 (find-next-state (make-fs abcd-machine "white") "b")
 "not found: white, b")
(check-expect
 (find-next-state (make-fs abcd-machine "yellow") "d")
 (make-fs abcd-machine "green"))
(define (find-next-state an-fsm ke)
  (make-fs
   (fs-fsm an-fsm)
   (find (fs-fsm an-fsm) (fs-current an-fsm) ke)))

; FSM FSM-State KeyEvent -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field
(check-expect (find abcd-machine "white" "a") "yellow")
(check-error (find abcd-machine "black" "a") "not found: black, a")
(define (find transitions current ke)
  (cond
    [(empty? transitions) (error (string-append "not found: " current ", " ke))]
    [(and (state=? current (transition-current (first transitions)))
          (string=? ke (transition-key (first transitions))))
     (transition-next (first transitions))]
    [else (find (rest transitions) current ke)]))

; (simulate.v2 abcd-machine "white")
