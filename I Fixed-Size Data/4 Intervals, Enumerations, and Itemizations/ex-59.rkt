;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-59) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define SCENE-WIDTH 70)
(define SCENE-HEIGHT (* SCENE-WIDTH 3))
(define BACKG (empty-scene SCENE-WIDTH SCENE-HEIGHT "black"))
(define BULB-SIZE (- (/ SCENE-WIDTH 2) (/ SCENE-WIDTH 6)))
(define SHIFT-DISTANCE (/ SCENE-WIDTH 2))

; A TrafficLight is one four strings:
; - "green"
; - "yellow"
; - "red"
; - "red and yellow"
; represents the state of a traffic light

; TrafficLight -> TrafficLight
; yields the next state, given current state cs
(check-expect (tl-next "red") "red and yellow")
(check-expect (tl-next "red and yellow") "green")
(check-expect (tl-next "green") "yellow")
(check-expect (tl-next "yellow") "red")
(define (tl-next cs)
  (cond
    [(string=? cs "green") "yellow"]
    [(string=? cs "yellow") "red"]
    [(string=? cs "red") "red and yellow"]
    [(string=? cs "red and yellow") "green"]))

; TrafficLight -> Image
; renders the current state cs as an image
(define (tl-render current-state)
  (cond
    [(string=? current-state "green")
     (place-image (draw-bulb "green") SHIFT-DISTANCE (* SHIFT-DISTANCE 5)
                  BACKGROUND)]
    [(string=? current-state "yellow")
     (place-image (draw-bulb "yellow") SHIFT-DISTANCE (* SHIFT-DISTANCE 3)
                  BACKGROUND)]
    [(string=? current-state "red")
     (place-image (draw-bulb "red") SHIFT-DISTANCE SHIFT-DISTANCE
                  BACKGROUND)]
    [(string=? current-state "red and yellow")
     (place-image (draw-bulb "yellow") SHIFT-DISTANCE (* SHIFT-DISTANCE 3)
                  (place-image (draw-bulb "red")
                               SHIFT-DISTANCE SHIFT-DISTANCE BACKGROUND))]))

; TrafficLight -> TrafficLight
; simulates a clock-based Polish traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
    [to-draw tl-render]
    [on-tick tl-next 1]))

; String -> Image
; renders a turned-on bulb of given color
(define (draw-bulb color)
  (circle BULB-SIZE "solid" color))

(define BACKGROUND
  (place-image (draw-bulb "dim gray") SHIFT-DISTANCE (* SHIFT-DISTANCE 5)
               (place-image (draw-bulb "dim gray") SHIFT-DISTANCE (* SHIFT-DISTANCE 3)
                            (place-image (draw-bulb "dim gray")
                                         SHIFT-DISTANCE SHIFT-DISTANCE
                                         BACKG))))

(traffic-light-simulation "red")