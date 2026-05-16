;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname matrix) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 '())))

; Matrix -> Matrix
; transposes the given matrix along the diagonal

(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define tam1 (cons wor1 (cons wor2 '())))

(check-expect (transpose mat1) tam1)

(define (transpose lln)
  (cond
    [(empty? (first lln)) '()]
    [else (cons (first* lln) (transpose (rest* lln)))]))

; Matrix -> List-of-numbers
; consumes a matrix and produces the first column
; as a list of numbers
(check-expect (first* '()) '())
(check-expect (first* mat1) wor1)
(define (first* lln)
  (cond
    [(empty? lln) '()]
    [else (cons (first (first lln))
                (first* (rest lln)))]))

; Matrix -> Matrix
; removes first column from a matrix
(check-expect (rest* (cons (cons 1 '()) '())) (cons '() '()))
(check-expect (rest* (cons row1 '())) (cons (cons 12 '()) '()))
(check-expect (rest* mat1) (cons (cons 12 '()) (cons (cons 22 '()) '())))
(define (rest* lln)
  (cond
    [(empty? (rest lln)) (cons (rest (first lln)) '())]
    [else (cons (rest (first lln))
                (rest* (rest lln)))]))