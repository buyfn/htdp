#lang htdp/isl+
(require 2htdp/batch-io)
(require 2htdp/abstraction)
(require 2htdp/image)
(require 2htdp/universe)

(define PREFIX "https://stockanalysis.com/stocks/")
(define SIZE 22) ; font size 
 
(define-struct data [price delta])
; A StockWorld is a structure: (make-data String String)

; String -> StockWorld
; retrieves the stock price of co and its change every 15s
(define (stock-alert co)
  (local ((define url (string-append PREFIX co))
          ; [StockWorld -> StockWorld]
          ; fetches web page, scrapes stock price data and
          ; returns StockWorld value
          (define (retrieve-stock-data __w)
            (retrieve-stock-price co))
          ; StockWorld -> Image
          ; renders StockWorld data
          (define (render-stock-data w)
            (local (; [StockWorld String -> String] -> Image
                    ; renders text returned by `(sel w)` in color `col`
                    (define (word sel col)
                      (text (sel w) SIZE col)))
              (overlay (beside (word data-price 'black)
                               (text "  " SIZE 'white)
                               (word data-delta 'red))
                       (rectangle 300 35 'solid 'white)))))
    (big-bang (retrieve-stock-data 'no-use)
      [on-tick retrieve-stock-data 15]
      [to-draw render-stock-data])))

; String -> StockWorld
; retrieves stock data for the given company
(define (retrieve-stock-price co)
  (local ((define url (string-append PREFIX co "/"))
          (define page (read-xexpr/web url)))
    (make-data
     (first (xexpr-content (find-xexpr page price-el?)))
     (first (xexpr-content (find-xexpr page price-delta-el?))))))

(define (div? xexpr)
  (symbol=? (xexpr-name xexpr) 'div))

; Xexpr [Xexpr -> Boolean] -> [Maybe Xexpr]
; recursively searches xexpr, produces the first child that
; satisfies the predicate
(check-expect (find-xexpr '(main) div?) #false)
(check-expect (find-xexpr '(html '(body '(div "text"))) div?)
              '(div "text"))
(define (find-xexpr xexpr pred)
  (cond
    [(pred xexpr) xexpr]
    [(not (list? xexpr)) #false]
    [else (for/or ([child (xexpr-content xexpr)])
            (find-xexpr child pred))]))

; Xexpr -> Boolean
; returns true if given xexpr is a price element
(check-expect (price-el? '()) #false)
(check-expect (price-el? '(div "14.00")) #false)
(check-expect (price-el? '(div ((class "text-4xl")) "14.00")) #true)
(check-expect (price-el? '(div ((class "random")) "14.00")) #false)
(check-expect (price-el? '(div ((class "mb-0.5 text-center text-4xl font-semibold text-green-vivid")) "$15.78")) #true)
(define (price-el? xexpr)
  (and (list? xexpr)
       (not (empty? xexpr))
       (symbol=? (xexpr-name xexpr) 'div)
       (string? (find-attr (xexpr-attr xexpr) 'class))
       (string-contains? "text-4xl" (find-attr (xexpr-attr xexpr) 'class))))

; Xexpr -> Boolean
; returns true if given xexpr is a price delta element
(define (price-delta-el? xexpr)
  (and (list? xexpr)
       (not (empty? xexpr))
       (symbol=? (xexpr-name xexpr) 'div)
       (string? (find-attr (xexpr-attr xexpr) 'class))
       (string-contains? "sm:text-2xl" (find-attr (xexpr-attr xexpr) 'class))))
 
; Xexpr -> Symbol
; returns the name of xexpr
(define (xexpr-name xexpr) (first xexpr))

; Xexpr -> Attributes
; produces the attributes of an Xexpr
(check-expect (xexpr-attr '(div "14.00")) '())
(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))

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
    [(not (list? x)) #false]
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; Xexpr -> [List-of Xexpr]
; returns the content of xexpr
(define (xexpr-content xexpr)
  (local ((define optional-loa+content (rest xexpr)))
    (cond
      [(empty? optional-loa+content) '()]
      [(list-of-attributes? (first optional-loa+content))
       (rest optional-loa+content)]
      [else optional-loa+content])))

