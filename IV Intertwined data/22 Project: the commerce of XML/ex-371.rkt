#lang htdp/isl+
; An Xexpr is on of:
; - (cons Symbol XexprContent)
; - String

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
