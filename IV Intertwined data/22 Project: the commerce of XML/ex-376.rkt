#lang htdp/isl+
(require 2htdp/image)

; An XItem.v2 is one of:
; - (cons 'li (cons XWord '()))
; - (cons 'li (cons [List-of Attribute] (cons XWord '())))
; - (cons 'li (cons XEnum.v2 '()))
; - (cons 'li (cons [List-of Attribute] (cons XEnum.v2 '())))

; An XEnum.v2 is one of:
; - (cons 'ul [List-of XItem.v2])
; - (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))

(define e0
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "hello"))))))
(define e1
  `(ul
    (li (word ((text "one"))))
    (li ,e0)
    (li (word ((text "hello"))))))

; XEnum.v2 -> Number
; counts all "hello"s in ex
(check-expect (count-hello e0) 1)
(check-expect (count-hello e1) 2)
(define (count-hello ex)
  (local ((define content (xexpr-content ex))
          ; XItem.v2 Number -> Number
          (define (count-one item sum)
            (+ (count-hello/item item) sum)))
    (foldl count-one 0 content)))

; XItem.v2 -> Number
; counts all "hello"s in item
(define (count-hello/item item)
  (cond
    [(word? item) (if (string=? (word-text item) "hello") 1 0)]
    [else (count-hello item)]))

; Xexpr -> Symbol
; returns the name of xexpr
(define (xexpr-name xexpr) (first xexpr))

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

; Xexpr -> [List-of Xexpr]
; returns the content of xexpr
(define (xexpr-content xexpr)
  (local ((define optional-loa+content (rest xexpr)))
    (cond
      [(empty? optional-loa+content) '()]
      [(list-of-attributes? (first optional-loa+content))
       (rest optional-loa+content)]
      [else optional-loa+content])))

; AttributesOrXexpr -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; Any -> Boolean
; is the value an XWord?
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
