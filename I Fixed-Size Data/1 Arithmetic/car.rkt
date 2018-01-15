;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname car) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; 1. a.
(define WIDTH-OF-WORLD 200)
(define HEIGHT-OF-WORLD 100)

(define WHEEL-RADIUS 10)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 6))
(define WHEEL-COLOR "black")
(define CAR-WIDTH (+ WHEEL-DISTANCE (* WHEEL-RADIUS 4)))
(define CAR-HEIGHT (* WHEEL-RADIUS 3))
(define CAR-PROPORTION 7/4)
(define CAR-COLOR "blue")
(define CLEARANCE (- 0 (* WHEEL-RADIUS 1/2)))
(define Y-CAR (/ HEIGHT-OF-WORLD 2))

; 1. b.
(define BACKGROUND (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD))

(define WHEEL
  (circle WHEEL-RADIUS "solid" WHEEL-COLOR))
(define BOTH-WHEELS
  (overlay/offset WHEEL WHEEL-DISTANCE 0 WHEEL))
(define CAR-BODY
  (overlay/align "middle" "bottom"
                 (rectangle (/ CAR-WIDTH CAR-PROPORTION)
                            CAR-HEIGHT
                            "solid" CAR-COLOR)
                 (rectangle CAR-WIDTH
                            (/ CAR-HEIGHT CAR-PROPORTION)
                            "solid" CAR-COLOR)))
(define CAR
  (overlay/align/offset "middle" "bottom"
                        BOTH-WHEELS
                        0
                        CLEARANCE
                        CAR-BODY))

; 2.
; A WorldState is a Number
; interpretation: the number of pixels between
; the left border of the scene and the car

; 3
; WorldState -> Image
; places the image of the car x pixels from
; the left margin of the BACKGROUND image
; examples:
;   given: 0, expect: (place-image CAR 0 Y-CAR BACKGROUND)
(define (render ws)
  (place-image CAR ws Y-CAR BACKGROUND))

; WorldState -> WorldState
; adds 3 to x to move the car right
(define (tock ws) (+ ws 3))

; WorldState -> Boolean
; after each event, big-bang evalueates (end? cw)
(define (end? cw) ...)

; WorldState -> WorldState
; launches the program from some initial state
(define (main ws)
  (big-bang ws
    [on-tick tock]
    [to-draw render]))

(main 0)