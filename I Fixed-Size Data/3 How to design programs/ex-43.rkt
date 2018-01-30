;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-43) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; An AnimationState is a Number.
; interpretation the number of clock ticks 
; since the animation started

(define WIDTH-OF-WORLD 600)
(define HEIGHT-OF-WORLD 400)
(define BACKGROUND (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD))
(define SIN-SCALE 10)

(define CAR-COLOR "red")
(define WHEEL-R 20)
(define WHEEL-W (/ WHEEL-R 2))
(define CAR-WIDTH (* WHEEL-R 3))
(define CAR-LENGTH (* WHEEL-R 6))

(define WINDSHIELD (rectangle WHEEL-R (- CAR-WIDTH WHEEL-W) "solid" "gray"))
(define REAR-WINDSHIELD (rectangle WHEEL-W (- CAR-WIDTH WHEEL-W) "solid" "gray"))
(define WINDOWS (overlay/offset REAR-WINDSHIELD
                                (* 2 WHEEL-R) 0
                                WINDSHIELD))
(define CAR-BODY (overlay/offset WINDOWS
                                 WHEEL-W 0
                                 (rectangle CAR-LENGTH CAR-WIDTH "solid" CAR-COLOR)))
(define WHEEL (rectangle WHEEL-R WHEEL-W "solid" "black"))
(define PAIR-OF-WHEELS (overlay/offset WHEEL 0 CAR-WIDTH WHEEL))
(define SET-OF-WHEELS (overlay/offset PAIR-OF-WHEELS
                                      (- CAR-LENGTH (* WHEEL-R 2)) 0
                                      PAIR-OF-WHEELS))
(define CAR (overlay/offset CAR-BODY 0 0 SET-OF-WHEELS))

; AnimationState -> Image
; places image of a car in the scene
; according to the AnimationState
; examples: 
(define (render as)
  (place-image (rotate
                (rad->deg (/ (asin (- 0 (cos (map-to-circle as)))) 2))
                CAR)
               as
               (+ (/ HEIGHT-OF-WORLD 2)
                  (* SIN-SCALE (sin (map-to-circle as))))
               BACKGROUND))

; Number -> Number
; Maps some number in interval [0; WIDTH-OF-WORLD]
; to another number in interval [0; 2PI]
; examples:
;   given: 0; expect: 0
;   given: WIDTH-OF-WORLD; expect: 2PI
;   given: 10; expect: (10 * 2pi) / WIDTH-OF-WORLD
(define (map-to-circle x)
  (/ (* x 2 pi)
     WIDTH-OF-WORLD))

; Radians -> Degrees
; convers angle from radians to degrees
(define (rad->deg r)
  (/ (* 360 r) (* 2 pi)))

; AnimationState -> AnimationState
; moves the car according to a sine wave
(define (tock as)
  (+ as 1))

; AnimationState -> Boolean
; returns true when car reaches right edge of the scene
(define (end? as)
  (> as WIDTH-OF-WORLD))

(define (main as)
  (big-bang as
    [on-tick tock]
    [to-draw render]
    [stop-when end?]))

(main 0)