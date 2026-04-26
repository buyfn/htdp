;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-102) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; A Box is a structure
; (make-box Number Number Number)
; interpretation (make-box w d h) specifies a box with
; - width of w
; - depth of d
; - height of h
(define-struct box [width depth height])

; A Spider is a structure
; (make-spider Number Box)
; interpretation (make-spider l b) specifies a spider with
; - l amount of legs
; - b space required for transportation
(define-struct spider [legs box-size])

; An Elephant is a Box
; intepretation specifeis a box size required to transport the elephant

; A Boa is a struct
; (make-boa Number Number)
; interpretation (make-boa l g) specifies a boa with
; - length l
; - girth g
(define-struct boa [length girth])

; An Armadillo is a struct
; (make-armadillo String Box)
; interpretation (make-armadillo n b) specifies an armadillo with
; - name n
; - space required for transportation b
(define-struct armadillo [name box-size])

; A ZooAnimal is one of
; - Spider
; - Elephant
; - Boa
; - Armadillo

; ZooAnimal Box -> Boolean
; Returns true if given animal fits inside of specified cage
(define (fits? animal cage)
  (cond
    [(spider? animal)
     (fits-box? (spider-box-size animal) cage)]
    [(box? animal)
     (fits-box? animal cage)]
    [(boa? animal)
     (fits-box? (make-box (boa-length animal)
                          (boa-girth animal)
                          (boa-girth animal))
                cage)]
    [(armadillo? animal)
     (fits-box? (armadillo-box-size animal) cage)]))

; Box Box -> Boolean
; Returns true if the first box fits inside of second box
(define (fits-box? b1 b2)
  (and (<= (box-width b1) (box-width b2))
       (<= (box-depth b1) (box-width b2))
       (<= (box-height b1) (box-height b2))))
