#lang htdp/isl+

; An Xexpr is a list:
; (cons Symbol XexprContent)

; An XexprContent is one of:
; - Body
; - (cons Attributes Body)

; A Body is one of:
; - '()
; - (cons Xexpr Body)

; An Attributes is one of:
; - '()
; - (cons Attribute Attributes)

; An Attribute is a list of two items:
; (cons Symbol (cons String '()))

; An AttributesOrXexpr is one of:
; - Attributes
; - Xexpr

; AttributesOrXexpr -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))
