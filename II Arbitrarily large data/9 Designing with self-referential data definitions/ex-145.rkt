;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-145) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; A CTemperature is a Number greater than -272

; An NEList-of-temperatures is one of:
; - (cons CTemperature '())
; - (cons CTemperature NEList-of-temperatures)

; NEList-of-temperatures -> Boolean
; determines whether list of temperatures is sorted in descending order
(check-expect (sorted? (cons 1 (cons 2 '()))) #false)
(check-expect (sorted? (cons 2 (cons 1 '()))) #true)
(define (sorted? ne-l)
  (cond
    [(empty? (rest ne-l)) #true]
    [else (and (> (first ne-l)
                  (first (rest ne-l)))
               (sorted? (rest ne-l)))]))