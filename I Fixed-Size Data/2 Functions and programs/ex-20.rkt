;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-20) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; function can't deal with empty strings, since i should be both
; less than str length and non-negative.
; For empty string of length 0 it's not possible
(define (string-delete str i) (string-append (substring str 0 i)
                                             (substring str (add1 i)
                                                        (string-length str))))