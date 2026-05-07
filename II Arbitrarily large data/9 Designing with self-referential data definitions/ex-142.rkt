;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-142) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; ImageOrFalse is one of:
; - Image
; - #false

; List-of-images, Number -> ImageOrFalse
; produces the first image on li that is not an n by n square,
; or #false if there's no such image
(define (ill-sized? loi n)
  (cond
    [(empty? loi) #false]
    [(ill-sized-single? (first loi) n) (first loi)]
    [else (ill-sized? (rest loi) n)]))

; Image, Number -> Boolean
(define (ill-sized-single? img n)
  (not (and (= (image-width img) n)
            (= (image-height img) n))))

(define SQUARE (empty-scene 20 20))