;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-163) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Number -> Number
; converts single Fahrenheit measurement
; to Celsius
(check-expect (f-to-c 32) 0)
(check-expect (f-to-c 212) 100)
(check-expect (f-to-c 14) -10)
(define (f-to-c m)
  (/ (* 5 (- m 32)) 9))

; List-of-numbers -> List-of-numbers
; converts a list of measurements in Fahrenheit
; to a list of Celsius measurements
(check-expect (convertFC '()) '())
(check-expect (convertFC (cons 32 (cons 212 '()))) (cons 0 (cons 100 '())))
(check-expect (convertFC (cons 14 (cons 50 '()))) (cons -10 (cons 10 '())))
(define (convertFC lof)
  (cond
    [(empty? lof) '()]
    [else (cons (f-to-c (first lof))
                (convertFC (rest lof)))]))
