;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-34) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 1. Data definitions
; We use 1strings to represent single characters

; 2. Signature, purpose, function header
; String -> 1string
; function extracts the first character from a non-empty string
; (define (string-first str) "a")

; 3. Functional examples
; given: "hello", expect: "h"
; given: "a",     expect: "a"

; 4. Function template
(define (sting-first str)
  (... str ...))

; 5. Function definition
(define (string-first str)
  (string-ith str 0))

; 6. Testing
(string-first "hello") ; "h"
(string-first "a")     ; "a"