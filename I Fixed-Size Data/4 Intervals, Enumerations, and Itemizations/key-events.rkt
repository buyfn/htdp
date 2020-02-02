;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname events) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define SENSITIVITY 3)
(define RADIUS 10)
(define SCENE-WIDTH 100)
(define SCENE-HEIGHT 100)
(define DOT-COLOR "red")
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))

; A WorldState is a number.
; It represents x coordinate of the dot

; WorldState -> WorldState
; launches the program from some initial state
(define (main ws)
  (big-bang ws
    [on-key handle-key-press]
    [to-draw render]))

; WorldState -> Image
; renders scene
(define (render ws)
  (place-image
   (circle RADIUS "solid" DOT-COLOR)
   ws
   (/ SCENE-HEIGHT 2)
   SCENE))

; WorldState, KeyEvent -> WorldState
; moves dot to the left or right depending on which key is pressed

(check-expect (handle-key-press 10 "left") (- 10 SENSITIVITY))
(check-expect (handle-key-press 10 "right") (+ 10 SENSITIVITY))
(check-expect (handle-key-press 10 "l") 10)

(define (handle-key-press ws key)
  (cond
    [(string=? "left" key)
     (- ws SENSITIVITY)]
    [(string=? "right" key)
     (+ ws SENSITIVITY)]
    [else ws]))

(main (/ SCENE-WIDTH 2))