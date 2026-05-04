;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-134) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; String, List-of-strings -> Boolean
; Returns true if given string occurs in the list of strings
(check-expect (contains? "Flatt" (cons "b" (cons "Flatt" '()))) #true)
(check-expect (contains? "Flatt" '()) #false)
;; first position match
(check-expect (contains? "a" (cons "a" (cons "b" '()))) #true)
;; last position match
(check-expect (contains? "c" (cons "a" (cons "b" (cons "c" '())))) #true)
;; single-element list
(check-expect (contains? "x" (cons "x" '())) #true)
(check-expect (contains? "y" (cons "x" '())) #false)
;; duplicates
(check-expect (contains? "dup" (cons "dup" (cons "dup" '()))) #true)
;; case-sensitive
(check-expect (contains? "flatt" (cons "Flatt" '())) #false)
(define (contains? s ss)
  (cond
    [(empty? ss) #false]
    [(string=? (first ss) s) #true]
    [else (contains? s (rest ss))]))
