#lang htdp/isl+
(require 2htdp/universe)
(require 2htdp/image)

(define WIDTH 200)
(define HEIGHT 400)
(define V 1)

(define ROCKET (rectangle 5 20 "solid" "blue"))
(define ROCKET-CENTER-TO-TOP
  (- HEIGHT (/ (image-height ROCKET) 2)))

; An ImageStream is a function:
; [N -> Image]
; interpretation a stream s denotes a series of images

; ImageStream
(define (picture-of-rocket t)
  (cond
    [(<= (distance t) ROCKET-CENTER-TO-TOP)
     (place-image ROCKET (/ WIDTH 2)
                  (distance t) (empty-scene WIDTH HEIGHT))]
    [(> (distance t) ROCKET-CENTER-TO-TOP)
     (place-image ROCKET
                  (/ WIDTH 2) ROCKET-CENTER-TO-TOP
                  (empty-scene WIDTH HEIGHT))]))

(define (distance t)
  (* V t))

; ImageStream N -> N
; shows the image (s 0), (s 1), and so on
; at a rate of 30 images per second up to n
; images total
(define (my-animate s n)
  (big-bang 0
            [to-draw s]
            [on-tick add1 (/ 1 30)]
            [stop-when
             (lambda (x) (>= x n))]))

; (my-animate picture-of-rocket 200)
