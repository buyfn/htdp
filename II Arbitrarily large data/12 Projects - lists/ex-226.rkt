;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-226) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))

; An FSM is one of:
; - '()
; - (cons Transition FSM)

(define-struct transition [current next])
; A Transition is a structure:
; (make-transition FSM-State FSM-State)

; FSM-State is a Color

; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another
; in reaction to keystrokes

; A SimulateionState.v1 is an FSM-State

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

(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))

(define bw-machine
  (list (make-transition "black" "white")
        (make-transition "white" "black")))

; FSM -> SimulationState.v1
; match the keys pressed with the given FSM
(define (simulate.v1 fsm0)
  (big-bang initial-state
            [to-draw render-state.v1]
            [on-key find-next-state.v1]))

; SimulationState.v1 -> Image
; renders a wolrd state as an image
(define (render-state.v1 s)
  empty-image)

; SimulationState.v1 KeyEvent -> SimulationState.v1
; finds the next state from ke and cs
(define (find-next-state.v1 cs ke)
  cs)
