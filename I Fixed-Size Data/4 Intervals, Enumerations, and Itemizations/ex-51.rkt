;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-51) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define RADIUS 30)
(define SCENE-WIDTH 100)
(define SCENE-HEIGHT 100)
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))

; A WorldState is on of these strings:
; - red
; - yellow
; - green

; WorldState -> Image
; creates a circle of appropriate color
; examples:
;   given: "green", expect: (circle RADIUS "solid" "green")
(define (render ws)
  (place-image
   (circle RADIUS "solid" ws)
   (/ SCENE-WIDTH 2)
   (/ SCENE-HEIGHT 2)
   SCENE))

; WorldState -> WorldState
; switches to the next traffic light
(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]))

; WorldState -> WorldState
; launches the program from some initial state
(define (main ws)
  (big-bang ws
    [on-tick traffic-light-next]
    [to-draw render]))

(main "yellow")