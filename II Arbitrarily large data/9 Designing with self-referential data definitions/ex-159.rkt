;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-159) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define BACKGROUND (empty-scene 80 180))

(define-struct pair [balloon# lob])
; A Pair is a structure (make-pair N List-of-posns)
; A List-of-posns is one of: 
; – '()
; – (cons Posn List-of-posns)
; interpretation (make-pair n lob) means n balloons 
; must yet be thrown and added to lob

; N -> Pair
(define (riot n)
  (big-bang (make-pair n '())
    [on-tick tock]
    [to-draw render]))

; Pair -> Image
; renders thrown balloons
(define (render ws)
  (add-balloons(pair-lob ws)))

; Pair -> Pair
; add one more thrown balloon
(define (tock ws)
  (cond
    [(= (pair-balloon# ws) 0) ws]
    [else (make-pair (- (pair-balloon# ws) 1)
                     (cons (make-posn (random 80)
                                      (random 180))
                           (pair-lob ws)))]))

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
(define BACKG (overlay (col 18 (row 8 CELL))
                       BACKGROUND))

; List-of-posn -> Image
(define (add-balloons lop)
  (cond
    [(empty? lop) BACKG]
    [else (place-image (circle 3 "solid" "red")
                       (posn-x (first lop))
                       (posn-y (first lop))
                       (add-balloons (rest lop)))]))
