;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-163) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

; List-of-posns -> List-of-posns
; increases y-coordinate by 1 for every posn in list
(check-expect (tranlate '()) '())
(check-expect (tranlate (cons (make-posn 1 2) '()))
              (cons (make-posn 1 3) '()))
(check-expect (tranlate (cons (make-posn 0 0)
                              (cons (make-posn 4 -2) '())))
              (cons (make-posn 0 1)
                    (cons (make-posn 4 -1) '())))
(define (tranlate lop)
    (cond
        [(empty? lop) '()]
        [else (cons (make-posn (posn-x (first lop))
                               (+ (posn-y (first lop)) 1))
                    (tranlate (rest lop)))]))
