;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-147) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; NEList-of-booleans is one of:
; - (cons Boolean '())
; - (cons Boolean NEList-of-booleans)

; List-of-booleans -> Boolean
; determines whether all values in list are #true
(check-expect (all-true (cons #true '())) #true)
(check-expect (all-true (cons #true (cons #false '()))) #false)
(check-expect (all-true (cons #true (cons #true '()))) #true)
(define (all-true ne-l)
  (cond
    [(empty? (rest ne-l)) (first ne-l)]
    [else (and (first ne-l)
               (all-true (rest ne-l)))]))

; List-of-booleans -> Boolean
; determines wheter at least one values in list is #true
(check-expect (one-true (cons #true '())) #true)
(check-expect (one-true (cons #false (cons #false '()))) #false)
(check-expect (one-true (cons #true (cons #false '()))) #true)
(check-expect (one-true (cons #true (cons #true '()))) #true)
(define (one-true ne-l)
  (cond
    [(empty? (rest ne-l)) (first ne-l)]
    [else (or (first ne-l)
              (one-true (rest ne-l)))]))