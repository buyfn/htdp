;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-140) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; List-of-booleans -> Boolean
; determines whether all values in list are #true
(check-expect (all-true '()) #true)
(check-expect (all-true (cons #true '())) #true)
(check-expect (all-true (cons #true (cons #false '()))) #false)
(check-expect (all-true (cons #true (cons #true '()))) #true)
(define (all-true lob)
  (cond
    [(empty? lob) #true]
    [else (and (first lob)
               (all-true (rest lob)))]))

; List-of-booleans -> Boolean
; determines wheter at least one values in list is #true
(check-expect (one-true '()) #false)
(check-expect (one-true (cons #true '())) #true)
(check-expect (one-true (cons #false (cons #false '()))) #false)
(check-expect (one-true (cons #true (cons #false '()))) #true)
(check-expect (one-true (cons #true (cons #true '()))) #true)
(define (one-true lob)
  (cond
    [(empty? lob) #false]
    [else (or (first lob)
              (one-true (rest lob)))]))