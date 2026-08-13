#lang htdp/isl+
(require 2htdp/image)

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

; An XWord is '(word ((text String)))

; An AttributesOrXexpr is one of:
; - Attributes
; - Xexpr

; An XEnum.v1 is one of:
; - (cons 'ul [List-of XItem.v1])
; - (cons 'ul (cons Attributes [List-of XItem.v1]))
; An XItem.v1 is one of:
; - (cons 'li (cons Xword '()))
; - (cons 'li (cons Attributes (cons XWord '())))

(define BT (circle 2 "solid" "black"))

(define xword-1 '(word ((text "text 1"))))
(define xword-2 '(word ((text "another text"))))
(define xword-3 '(word ((text "yet another text"))))

; XItem.v1 -> Image
; renders an item as a "word" prefixed by a bullet
; the function extracts the text value of the word in the item element,
; and renders a bullet point BT beside the rendered text
(check-expect (render-item1 '(li (word ((text "one")))))
              (beside/align 'center
                            (circle 2 "solid" "black")
                            (text "one" 12 'black)))
(define (render-item1 i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word (word-text element))
          (define item (text a-word 12 'black)))
    (beside/align 'center BT item)))

; Any -> Boolean
; is the value an XWord?
(check-expect (word? xword-1) #true)
(check-expect (word? "word") #false)
(check-expect (word? '(text ((text "text")))) #false)
(check-expect (word? '(word ((non "sense")))) #false)
(check-expect (word? '(word ((text "text")) '(extra))) #false)
(check-expect (word? (list 1)) #false)
(check-expect (word? identity) #false)
(check-expect (word? '(word ((text)))) #false)
(check-expect (word? '(word ((text "text" "extra")))) #false)
(check-expect (word? '(word)) #false)
(check-expect (word? '(word 1)) #false)
(check-expect (word? '(word ())) #false)
(check-expect (word? '(word ((1 "text")))) #false)
(check-expect (word? '(word (("text" "text")))) #false)
(check-expect (word? '(word ((text 42)))) #false)
(check-expect (word? '(word ((text "text") (other "value")))) #false)
(check-expect (word? '(word (word 1))) #false)
(define (word? x)
  (cond
    [(atom? x) #false]
    [(empty? x) #false]
    [else
     (and (cons? x)
          (symbol? (xexpr-name x))
          (symbol=? (xexpr-name x) 'word)
          (= (length x) 2)
          (ormap cons? (rest x))
          (xword-attributes? (xexpr-attr x)))]))

; XWord -> String
; produces the text of an XWord
(check-expect (word-text xword-1) "text 1")
(check-expect (word-text xword-2) "another text")
(check-expect (word-text xword-3) "yet another text")
(define (word-text word)
  (find-attr (xexpr-attr word) 'text))

; Attributes -> Boolean
; are given attributes correct XWord attributes?
(define (xword-attributes? xw)
  (and (= (length xw) 1)
       (cons? (first xw))
       (= (length (first xw)) 2)
       (symbol? (first (first xw)))
       (symbol=? (first (first xw))
                 'text)
       (string? (second (first xw)))))

; Attributes Symbol -> [Maybe String]
; If attribute x exists in attrs, returns it's value,
; otherwise returns #false
(check-expect (find-attr '((a "value 1")
                           (b "value 2"))
                         'a)
              "value 1")
(check-expect (find-attr '((a "value 1")
                           (b "value 2"))
                         'c)
              #false)
(define (find-attr attrs x)
  (local ((define maybe-association (assq x attrs)))
    (if (false? maybe-association)
        #false
        (second maybe-association))))

; Xexpr -> Symbol
; returns the name of xexpr
(define (xexpr-name xexpr) (first xexpr))

; Xexpr -> [List-of Xexpr]
; returns the content of xexpr
(define (xexpr-content xexpr)
  (local ((define optional-loa+content (rest xexpr)))
    (cond
      [(empty? optional-loa+content) '()]
      [(list-of-attributes? (first optional-loa+content))
       (rest optional-loa+content)]
      [else optional-loa+content])))

; Xexpr -> Attributes
; produces the attributes of an Xexpr
(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))

; Xexpr -> [List-of Symbol]
; produces a list of attrube names of xexpr
(define (xexpr-attribute-names xe)
  (map (lambda (attr) (first attr)) (xexpr-attr xe)))

; AttributesOrXexpr -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; Any -> Boolean
; determines whether passed value is an atom
(check-expect (atom? 1) #true)
(check-expect (atom? "hi") #true)
(check-expect (atom? 'hi) #true)
(check-expect (atom? (list 1 2)) #false)
(check-expect (atom? identity) #false)
(define (atom? v)
  (or (number? v)
      (string? v)
      (boolean? v)
      (symbol? v)))
