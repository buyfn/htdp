;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-39) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define WHEEL-RADIUS 40)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 6))
(define WHEEL-COLOR "black")
(define CAR-WIDTH (+ WHEEL-DISTANCE (* WHEEL-RADIUS 4)))
(define CAR-HEIGHT (* WHEEL-RADIUS 3))
(define CAR-PROPORTION 7/4)
(define CAR-COLOR "blue")
(define CLEARANCE (- 0 (* WHEEL-RADIUS 1/2)))

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

CAR