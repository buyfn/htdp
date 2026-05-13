;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-163) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

; List-of-posns -> List-of-posns
; returns only those posns from input whose x-coordinates
; are between 0 and 100 and
; y-coordinates are between 0 and 200
(check-expect (legal '()) '())
(check-expect (legal (cons (make-posn 50 100) '()))
              (cons (make-posn 50 100) '()))
(check-expect (legal (cons (make-posn 150 100) '()))
              '())
(check-expect (legal (cons (make-posn 50 250) '()))
              '())
(check-expect (legal (cons (make-posn 0 0)
                           (cons (make-posn 100 200)
                                 (cons (make-posn -1 100)
                                       (cons (make-posn 50 300) '())))))
              (cons (make-posn 0 0)
                    (cons (make-posn 100 200) '())))
(define (legal lop)
    (cond
        [(empty? lop) '()]
        [(and (<= 0 (posn-x (first lop)) 100)
              (<= 0 (posn-y (first lop)) 200))
         (cons (first lop) (legal (rest lop)))]
        [else (legal (rest lop))]))
