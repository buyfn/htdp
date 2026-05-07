;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-139) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; A List-of-amounts is one of:
; - '()
; - (cons PositiveNumber List-of-amounts)
; examples:
; - '()
; - (cons 1 '())
; - (cons 1 (cons 2 '()))

; List-of-amounts -> Number
; computes the sum of list of amounts
(check-expect (sum '()) 0)
(check-expect (sum (cons 1 '())) 1)
(check-expect (sum (cons 1 (cons 2 '()))) 3)
(define (sum loa)
  (cond
    [(empty? loa) 0]
    [else (+ (first loa)
             (sum (rest loa)))]))

; A List-of-numbers is one of:
; - '()
; - (cons Number List-of-numbers)

; List-of-numbers -> Boolean
; determines whether all numbers in list are positive
(check-expect (pos? '()) #true)
(check-expect (pos? (cons 5 '())) #true)
(check-expect (pos? (cons -1 '())) #false)
(define (pos? lon)
  (cond
    [(empty? lon) #true]
    [else (and (> (first lon) 0)
               (pos? (rest lon)))]))

; List-of-numbers -> Number
(check-expect (checked-sum (cons 1 (cons 2 '()))) 3)
(check-error (checked-sum (cons -1 '())) "checked-sum: list-of-amounts expected")
(define (checked-sum lon)
  (cond
    [(pos? lon) (sum lon)]
    [else (error "checked-sum: list-of-amounts expected")]))
