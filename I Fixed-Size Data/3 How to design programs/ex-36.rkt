;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-36) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; 1. Data definitions
; Area of an image is a Number
; interpretation represents number of pixels

; 2. Signature, purpose, function header
; Image -> Number
; counts the number of pixels in a given image
; (define (image-area img) 0)

; 3. Functional examples
; given: (empty-scene 10 10), expect: 100
; given: (empty-scene 100 2), expect: 200

; 4. Function template
; (define (image-area img)
;   (... (... img)
;        (... img)))

; 5. Function definition
(define (image-area img)
  (* (image-width img)
     (image-height img)))

; 6. Testing
(image-area (empty-scene 10 10)) ; 100
(image-area (empty-scene 100 2)) ; 200