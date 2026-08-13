#lang htdp/isl+
(require 2htdp/image)

(define SIZE 12) ; font size
(define COLOR "black") ; font color
(define BT ; a graphical constant
  (beside (circle 1 'solid COLOR)
          (text " " SIZE COLOR)))
(define SPACER ; a spacer graphical constant
  (square SIZE "solid" (make-color 0 0 0 0)))

; An Xitem.v2 is one of:
; - (cons 'li (cons XWord '()))
; - (cons 'li (cons [List-of Attribute] (cons XWord '())))
; - (cons 'li (cons XEnum.v2 '()))
; - (cons 'li (cons [List-of Attribute] (cons XEnum.v2 '())))

; An Xenum.v2 is one of:
; - (cons 'ul [List-of XItem.v2])
; - (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))

(define e0
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))
(define e1
  `(ul
    (li (word ((text "one"))))
    (li ,e0)
    (li (word ((text "three"))))))

(define e0-rendered
  (above/align
   'left
   (beside/align 'center BT (text "one" 12 'black))
   (beside/align 'center BT (text "two" 12 'black))))

; XEnum.v2 -> Image
; renders an XEnum.v2 as an image
(check-expect (render-enum e0) e0-rendered)
(define (render-enum xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v2 Image -> Image
          (define (deal-with-one item so-far)
            (above/align 'left (render-item item) so-far)))
    (foldr deal-with-one empty-image content)))

; XItem.v2 -> Image
; renders one XItem.v2 as an image
(define (render-item an-item)
  (local ((define content (first (xexpr-content an-item))))
    (cond
      [(word? content) (bulletize (text (word-text content) SIZE COLOR))]
      [else (indent (render-enum content))])))

; Image -> Image
; marks item with bullet
(define (bulletize item)
  (beside/align 'center BT item))

; Image -> Image
; indents item
(define (indent item)
  (beside/align 'center SPACER item))

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

; AttributesOrXexpr -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))
