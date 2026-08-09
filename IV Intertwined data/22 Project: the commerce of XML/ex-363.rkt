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


;; Exercise 364

; Can't be represented by either Xexpr.v0 (lacks attributes and body),
; or Xexpr.v1 (lacks attributes)
'(transition ((from "seen-e") (to "seen-f")))

; Can be represented by Xexpr.v1,
; can't be represented by Xexpr.v0 (lacks content)
'(ul (li (word) (word))
     (li (word)))


;; Exercise 365

'(server ((name "example.org")))
;; <server name="example.org" />
;; is not an element of either Xexpr.v0 or Xexpr.v1

'(carcas (board (grass)) (player ((name "sam"))))
;; <carcas>
;;   <board><grass /></board>
;;   <player name="sam" />
;; </carcas>
;; is not an element of either Xexpr.v0 or Xexpr.v1

'(start)
;; <start />
;; is an element of both Xexpr.v0 and Xexpr.v1
