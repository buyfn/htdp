;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define HEIGHT 10)
(define WIDTH (/ HEIGHT 5))
(define RADIUS (/ HEIGHT 2))

(overlay/xy (circle RADIUS "solid" "green")
            (- RADIUS (/ WIDTH 2)) (/ HEIGHT 2)
            (rectangle WIDTH HEIGHT "solid" "brown"))