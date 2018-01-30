;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-37) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 1. Data definitions
; Strings represent Strings

; 2. Signature, purpose, function header
; String -> String
; produces a string like the given one
; with the first character removed
; (define (string-rest str) "a")

; 3. Functional examples
; given: "hello", expect: "ello"
; given: "a",     expect: ""

; 4. Function template
; (define (string-rest str)
;   (... str ...))

; 5. Function definition
(define (string-rest str)
  (substring str 1 (string-length str)))
             
; 6. Testing
(string-rest "hello") ; "ello"
(string-rest "a")     ; ""