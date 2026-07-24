#lang htdp/isl+
(require 2htdp/abstraction)

(define-struct phone [area switch four])
; A Phone is a structure:
; (make-phone Number Number Number)
; represents a phone number

(define test-phones `(,(make-phone 713 123 4567) ,(make-phone 707 123 4567)))
(define expected-phones `(,(make-phone 281 123 4567) ,(make-phone 707 123 4567)))

(check-expect (replace '()) '())
(check-expect (replace test-phones) expected-phones)

; [List-of Phone] -> [List-of Phone]
; substitutes area code 713 with 281 in for every phone in lop
(define (replace lop)
  (for/list ((p lop))
    (match p
      [(phone 713 switch four) (make-phone 281 switch four)]
      [x x])))
