;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-163) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

(define USD/EUR 0.85)

; Number -> Number
; converts US$ amount to € amount
(check-expect (usd-to-eur 0) 0)
(check-expect (usd-to-eur 10) 8.5)
(check-expect (usd-to-eur 2.5) 2.125)
(define (usd-to-eur amount) (* amount USD/EUR))

; List-of-numbers -> List-of-numbers
; converts a list of US$ amounts into a list of € amounts
(check-expect (convert-euro '()) '())
(check-expect (convert-euro (cons 1.0 '())) (cons 0.85 '()))
(check-expect (convert-euro (cons 10 (cons 2.5 '()))) (cons 8.5 (cons 2.125 '())))
(define (convert-euro lod)
    (cond
        [(empty? lod) '()]
        [else (cons (usd-to-eur (first lod))
                    (convert-euro (rest lod)))]))

; Number List-of-numbers -> List-of-numbers
; consumes an exchange rate and a list of US$ amounts
; and converts the latter into a list of € amounts
(check-expect (convert-euro* USD/EUR '()) '())
(check-expect (convert-euro* USD/EUR (cons 1.0 '())) (cons 0.85 '()))
(check-expect (convert-euro* USD/EUR (cons 10 (cons 2.5 '()))) (cons 8.5 (cons 2.125 '())))
(define (convert-euro* rate lod)
    (cond
        [(empty? lod) '()]
        [else (cons (* (first lod) USD/EUR)
                    (convert-euro* rate (rest lod)))]))
