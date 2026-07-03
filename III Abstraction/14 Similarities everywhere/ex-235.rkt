;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex-235) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; String Los -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))

; Los -> Boolean
; determines whether l contains the string "atom"
(check-expect (contains-atom? '()) #false)
(check-expect (contains-atom? '("not an atom")) #false)
(check-expect (contains-atom? '("atom")) #true)
(check-expect (contains-atom? '("basic" "atom" "zoo")) #true)
(check-expect (contains-atom? '("first" "second" "third")) #false)
(define (contains-atom? l)
  (contains? "atom" l))

; Los -> Boolean
; determines whether l contains the string "basic"
(check-expect (contains-basic? '()) #false)
(check-expect (contains-basic? '("not basic")) #false)
(check-expect (contains-basic? '("basic")) #true)
(check-expect (contains-basic? '("atom" "basic" "zoo")) #true)
(check-expect (contains-basic? '("first" "second" "third")) #false)
(define (contains-basic? l)
  (contains? "basic" l))

; Los -> Boolean
; determines whether l contains the string "zoo"
(check-expect (contains-zoo? '()) #false)
(check-expect (contains-zoo? '("not zoo")) #false)
(check-expect (contains-zoo? '("zoo")) #true)
(check-expect (contains-zoo? '("atom" "zoo" "basic")) #true)
(check-expect (contains-zoo? '("first" "second" "third")) #false)
(define (contains-zoo? l)
  (contains? "zoo" l))
