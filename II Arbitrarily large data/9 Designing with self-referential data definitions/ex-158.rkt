;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-158) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define HEIGHT 220) ; distances in terms of pixels
(define WIDTH 30)
(define XSHOTS (- (/ WIDTH 2) 5))
 
; graphical constants 
(define BACKGROUND (empty-scene WIDTH HEIGHT "green"))
(define SHOT (isosceles-triangle 20 20 "solid" "black"))
(define SHOT-HEIGHT (image-height SHOT))

; A List-of-shots is one of: 
; – '()
; – (cons Shot List-of-shots)
; interpretation the collection of shots fired 

; A Shot is a Number.
; interpretation represents the shot's y-coordinate 

; A ShotWorld is List-of-numbers. 
; interpretation each number on such a list
;   represents the y-coordinate of a shot

; ShotWorld -> ShotWorld 
(define (main w0)
  (big-bang w0
    [on-tick tock]
    [on-key keyh]
    [to-draw to-image]))

; ShotWorld -> Image
; adds the image of a shot for each  y on w 
; at (MID,y) to the background image
(check-expect (to-image (cons 9 '()))
              (place-image SHOT XSHOTS 9 BACKGROUND))
(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT XSHOTS (first w)
                       (to-image (rest w)))]))

; ShotWorld -> ShotWorld
; moves each shot on w up by one pixel 
(check-expect (tock '()) '())
(check-expect (tock (cons 10 '())) (cons 9 '()))
(check-expect (tock (cons 15 (cons 3 '()))) (cons 14 (cons 2 '())))
(define (tock w)
  (cond
    [(empty? w) w]
    [else (remove-out-of-bounds (cons (- (first w) 1)
                                      (tock (rest w))))]))

; ShotWorld KeyEvent -> ShotWorld 
; adds a shot to the world 
; if the player presses the space bar
(check-expect (keyh '() " ") (cons HEIGHT '()))
(check-expect (keyh (cons 20 '()) " ") (cons HEIGHT (cons 20 '())))
(check-expect (keyh (cons 20 '()) "a") (cons 20 '()))
(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))

; List-of-shots -> List-of-shots
; removes shots that are placed outside of canvas from the list
(define (remove-out-of-bounds shots)
  (cond
    [(empty? shots) shots]
    [else (if (< (+ (first shots) SHOT-HEIGHT) 0)
              (remove-out-of-bounds (rest shots))
              (cons (first shots)
                    (remove-out-of-bounds (rest shots))))]))