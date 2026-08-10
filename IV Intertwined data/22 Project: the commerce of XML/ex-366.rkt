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

(define xexpr-1 '(transition ((from "seen-e") (to "seen-f"))))
(define xexpr-2 
  '(ul (li (word) (word))
       (li (word))))
(define xexpr-3 '(server ((name "example.org"))))
(define xexpr-4 
  '(carcas (board (grass)) (player ((name "sam")))))
(define xexpr-5 '(start))
(define xexpr-6 '(start ((when "ready")) (continue)))

; Xexpr -> Symbol
; returns the name of xexpr
(check-expect (xexpr-name xexpr-1) 'transition)
(check-expect (xexpr-name xexpr-2) 'ul)
(check-expect (xexpr-name xexpr-3) 'server)
(check-expect (xexpr-name xexpr-4) 'carcas)
(check-expect (xexpr-name xexpr-5) 'start)
(check-expect (xexpr-name xexpr-6) 'start)
(define (xexpr-name xexpr) (first xexpr))

; Xexpr -> [List-of Xexpr]
; returns the content of xexpr
(check-expect (xexpr-content xexpr-1) '())
(check-expect (xexpr-content xexpr-2) '((li (word) (word))
                                        (li (word))))
(check-expect (xexpr-content xexpr-3) '())
(check-expect (xexpr-content xexpr-4) '((board (grass)) (player ((name "sam")))))
(check-expect (xexpr-content xexpr-5) '())
(check-expect (xexpr-content xexpr-6) '((continue)))
(define (xexpr-content xexpr)
  (local ((define optional-loa+content (rest xexpr)))
    (cond
      [(empty? optional-loa+content) '()]
      [(list-of-attributes? (first optional-loa+content))
       (rest optional-loa+content)]
      [else optional-loa+content])))

; [List-of Attribute] or Xexpr -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))
