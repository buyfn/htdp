#lang htdp/isl+

(define-struct f-application [name arg])
(define-struct add [left right])
(define-struct mul [left right])

; A BSL-fun-expr is one of:
; - Number
; - Symbol
; - (make-f-application Symbol BSL-fun-expr)
; - (make-add BSL-fun-expr BSL-fun-expr)
; - (make-mul BSL-fun-expr BSL-fun-expr)

(make-f-application 'k (make-add 1 1))
(make-mul 5 (make-f-application 'k (make-add 1 1 )))
(make-mul (make-f-application 'i 5)
          (make-f-application 'k (make-add 1 1)))
