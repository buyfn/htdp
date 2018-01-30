;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-42) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define TREE
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))

(define BACKGROUND (underlay (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD)
                             TREE))

; 2.
; A WorldState is a Number
; interpretation: the number of pixels between
; the left border of the scene and right-most edge of the car

; 3
; WorldState -> Image
; places the image of the car to the scene
; according to the given WorldState
; examples:
(check-expect (render 0)
              (place-image CAR (- 0 (/ CAR-WIDTH 2)) Y-CAR BACKGROUND))
(define (render ws)
  (place-image CAR (- ws (/ CAR-WIDTH 2)) Y-CAR BACKGROUND))

; WorldState -> WorldState
; moves the car by 3 pixels for every clock tick
; examples:
(check-expect (tock 20) 23)
(check-expect (tock 78) 81)
(define (tock ws)
  (+ ws 3))

; WorldState -> Boolean
; returns #t when the car leaves the scene
; examples:
(check-expect (end? 200) (> 200 WIDTH-OF-WORLD))
(define (end? ws)
  (> ws WIDTH-OF-WORLD))

; WorldState -> WorldState
; launches the program from some initial state
(define (main ws)
  (big-bang ws
    [on-tick tock]
    [to-draw render]
    [stop-when end?]))

(main 0)