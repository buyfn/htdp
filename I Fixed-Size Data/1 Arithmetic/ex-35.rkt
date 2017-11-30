;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-35) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 1. Data definitions
; We use 1Strings to represent single characters

; 2. Signature, purpose, function header
; String -> 1string
; extracts the last character from a non-empty string
; (define (string-last str) "a")

; 3. Functional examples
; given: "hello", expect: "o"
; given: "a",     expect: "a"

; 4. Function template
; (define (sting-last str)
;   (... str ...))

; 5. Function definition
(define (string-last str)
  (string-ith str (sub1 (string-length str))))

; 6. Testing
(string-last "hello") ; "o"
(string-last "a")     ; "a"