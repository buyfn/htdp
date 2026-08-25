#lang htdp/isl+
(require 2htdp/abstraction)

(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)

; [List-of String] [List-of String] -> [List-of PhoneRecord]
; Combines names and phones into a list of PhoneRecords
; Assumptions:
; - input lists are of equal length
; - corresponding items belong to the same person
(check-expect (zip (list "john") (list "123"))
              (list (make-phone-record "john" "123")))
;; (define (zip lon lop)
;;   (cond
;;     [(empty? lon) '()]
;;     [else (cons (make-phone-record (first lon) (first lop))
;;                 (zip (rest lon) (rest lop)))]))
(define (zip lon lop)
  (for/list ((n lon)
             (p lop))
    (make-phone-record n p)))
