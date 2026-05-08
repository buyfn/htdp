;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-151) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; N, Image -> Image
; produces a column of n copies of images
(define (col n i)
  (cond
    [(= n 1) i]
    [else (above i (col (sub1 n) i))]))

; N, Image -> Image
; produces a row of n copies of images
(define (row n i)
  (cond
    [(= n 1) i]
    [else (beside i (row (sub1 n) i))]))

(define CELL (square 10 "outline" "black"))
(define BACKG (col 18 (row 8 CELL)))

; List-of-posn -> Image
(define (add-balloons lop)
  (cond
    [(empty? lop) BACKG]
    [else (place-image (circle 3 "solid" "red")
                       (posn-x (first lop))
                       (posn-y (first lop))
                       (add-balloons (rest lop)))]))

(define lob (cons (make-posn 70 30) (cons (make-posn 20 40) '())))

(add-balloons lob)
