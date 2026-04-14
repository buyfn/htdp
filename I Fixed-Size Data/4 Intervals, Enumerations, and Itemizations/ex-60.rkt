;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-60) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define SCENE-WIDTH 70)
(define SCENE-HEIGHT (* SCENE-WIDTH 3))
(define BACKG (empty-scene SCENE-WIDTH SCENE-HEIGHT "black"))
(define BULB-SIZE (- (/ SCENE-WIDTH 2) (/ SCENE-WIDTH 6)))
(define SHIFT-DISTANCE (/ SCENE-WIDTH 2))

; A TrafficLight is one of:
; - 0 interpretation the traffic light shows red
; - 1 interpretation the traffic light shows red-yellow
; - 2 interpretation the traffic light shows green
; - 3 interpretation the traffic light shows yellow

; TrafficLight -> TrafficLight
; yields the next state, given current state cs
(check-expect (tl-next 0) 1)
(check-expect (tl-next 1) 2)
(check-expect (tl-next 2) 3)
(check-expect (tl-next 3) 0)
(define (tl-next cs) (modulo (+ cs 1) 4))

; TrafficLight -> Image
; renders the current state cs as an image
(define (tl-render current-state)
  (cond
    [(= current-state 2)
     (place-image (draw-bulb "green") SHIFT-DISTANCE (* SHIFT-DISTANCE 5)
                  BACKGROUND)]
    [(= current-state 3)
     (place-image (draw-bulb "yellow") SHIFT-DISTANCE (* SHIFT-DISTANCE 3)
                  BACKGROUND)]
    [(= current-state 0)
     (place-image (draw-bulb "red") SHIFT-DISTANCE SHIFT-DISTANCE
                  BACKGROUND)]
    [(= current-state 1)
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

(traffic-light-simulation 0)

; numeric version conveys the intention less clearly,
; because state values are not clear without the explanation in data definition