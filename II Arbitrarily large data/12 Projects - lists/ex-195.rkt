;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-195) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define LOCATION "/usr/share/dict/words")

(define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings:
; - "a"
; - ...
; - "z"
; or, equivalently, a member? of this list:
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; 1String List-of-strings -> Number
; counts how many words in the given list start with the given letter
(check-expect (starts-with# "b" (list "yellow" "blue" "green" "brown")) 2)
(define (starts-with# letter dict)
  (cond
    [(empty? dict) 0]
    [(starts-with? letter (first dict))
     (+ 1 (starts-with# letter (rest dict)))]
    [else (starts-with# letter (rest dict))]))

; 1String String -> Boolean
; determines whether given word starts with given letter
(check-expect (starts-with? "b" "blue") #true)
(check-expect (starts-with? "b" "yellow") #false)
(define (starts-with? letter word)
  (string=? (string-ith word 0) letter))

(starts-with# "e" AS-LIST)
(starts-with# "z" AS-LIST)
