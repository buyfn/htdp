#lang htdp/isl+

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

; XEnum.v2 -> XEnum.v2
; replaces all "hello"s with "bye" in ex
(check-expect (replace-hello e0)
              '(ul (li (word ((text "one"))))
                   (li (word ((text "bye"))))))
(check-expect (replace-hello e1)
              '(ul (li (word ((text "one"))))
                   (li (ul (li (word ((text "one"))))
                           (li (word ((text "bye"))))))
                   (li (word ((text "bye"))))))
(check-expect
 (replace-hello
  '(ul ((id "greetings"))
       (li (word ((text "hello"))))
       (li (word ((text "one"))))))
 '(ul ((id "greetings"))
      (li (word ((text "bye"))))
      (li (word ((text "one"))))))
(check-expect
 (replace-hello
  '(ul ((id "outer"))
       (li (ul ((id "inner"))
               (li (word ((text "hello"))))))))
 '(ul ((id "outer"))
      (li (ul ((id "inner"))
              (li (word ((text "bye"))))))))
(define (replace-hello ex)
  (local ((define content (xexpr-content ex))
          (define attrs (xexpr-attr ex)))
    (cond
      [(empty? attrs)
       `(ul
         ,@(map replace-hello/item content))]
      [else
       `(ul ,attrs ,@(map replace-hello/item content))])))

; XItem.v2 -> XItem.v2
; replaces all "hello"s with "bye" in item
(check-expect
 (replace-hello/item
  '(li ((class "greeting"))
       (word ((text "hello")))))
 '(li ((class "greeting"))
      (word ((text "bye")))))
(check-expect
 (replace-hello/item
  '(li ((class "nested"))
       (ul (li (word ((text "hello")))))))
 '(li ((class "nested"))
      (ul (li (word ((text "bye")))))))
(define (replace-hello/item item)
  (local ((define content (first (xexpr-content item)))
          (define attrs (xexpr-attr item)))
    (cond
      [(word? content) (if (string=? (word-text content) "hello")
                           (if (empty? attrs)
                               '(li (word ((text "bye"))))
                               `(li ,attrs (word ((text "bye")))))
                           item)]
      [(empty? attrs) `(li ,(replace-hello content))]
      [else `(li ,attrs ,(replace-hello content))])))

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
