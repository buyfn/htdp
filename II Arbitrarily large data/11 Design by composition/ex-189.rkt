;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-189) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))

; Number List-of-numbers -> Boolean
; determines whether some number occurs in a list of numbers.
; Constraint: input list alon has to be sorted.
(check-expect (search-sorted 5 '()) #false)
(check-expect (search-sorted 5 (list 1 3 5 7 9)) #true)
(check-expect (search-sorted 4 (list 1 3 5 7 9)) #false)
(check-expect (search-sorted 0 (list 1 3 5 7 9)) #false)
(check-expect (search-sorted 10 (list 1 3 5 7 9)) #false)
(check-expect (search-sorted 5 (list 5)) #true)
(check-expect (search-sorted 5 (list 3)) #false)
(define (search-sorted n alon)
  (cond
    [(empty? alon) #false]
    [(> (first alon) n) #false]
    [(= (first alon) n) #true]
    [else (search-sorted n (rest alon))]))
