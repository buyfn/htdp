;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-172) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)

; A FileStat is a structure:
; (make-filestat Number Number Number)
; interpretation: (make-filestat chars words lines)
; represents the number characters, words and lines in a text file
(define-struct filestat [chars words lines])

; String -> FileStat
(define (ws n)
  (make-filestat (string-length (read-file n))
                 (length (read-words n))
                 (length (read-lines n))))
