;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-172) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)

; A List-of-strings is one of:
; - '()
; - (cons String List-of-strings)

; A LLS is one of:
; - '()
; - (cons List-of-strings LLS)

; LLS -> String
; converts a list of lines into a string
(check-expect (collapse '()) "")
(check-expect (collapse (list (list "hello" "world") (list "bye"))) "hello world\nbye")
(check-expect (collapse (list (list "a" "b") (list "c" "d"))) "a b\nc d")
(check-expect (collapse (list (list "onlyone"))) "onlyone")
(define (collapse lls)
    (cond
        [(empty? lls) ""]
        [(empty? (rest lls)) (collapse-line (first lls))]
        [else (string-append (collapse-line (first lls)) "\n"
                             (collapse (rest lls)))]))

; List-of-strings -> String
; converts a list of strings into a string
(check-expect (collapse-line '()) "")
(check-expect (collapse-line (list "hello" "world")) "hello world")
(check-expect (collapse-line (list "a" "b" "c")) "a b c")
(check-expect (collapse-line (list "single")) "single")
(define (collapse-line los)
    (cond
        [(empty? los) ""]
        [(empty? (rest los)) (first los)]
        [else (string-append (first los) " " (collapse-line (rest los)))]))

(write-file "ttt.dat"
            (collapse (read-words/line "ttt.txt")))
