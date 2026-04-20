;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-81) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; A Time is a stucture:
; (make-time Number Number Number)
; represents the amount of hours, minutes and seconds
; passed from midnight
(define-struct time [hours minutes seconds])

; Time -> Number
; Produces the number of seconds passed from midnight till t
(check-expect (time->seconds (make-time 12 30 2)) 45002)
(define (time->seconds t)
  (+ (* (time-hours t) 3600)
     (* (time-minutes t) 60)
     (time-seconds t)))