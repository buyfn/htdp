;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-163) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

; List-of-posns -> Number
; produces the sum of x-coordinates of all posns in the list
(check-expect (sum '()) 0)
(check-expect (sum (cons (make-posn 1 2) '())) 1)
(check-expect (sum (cons (make-posn 5 10) (cons (make-posn 2 4) '()))) 7)
(define (sum lop)
    (cond
        [(empty? lop) 0]
        [else (+ (posn-x (first lop))
                 (sum (rest lop)))]))
