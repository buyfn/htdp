;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-163) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

(define-struct phone [area switch four])
; A Phone is a structure:
; (make-phone Three Three Four)
; A Three is a Number between 100 and 999
; A four is a Number between 1000 and 9999

; List-of-phones -> List-of-phones
; replaces all occurrence of area code 713 with 281
(check-expect (replace '()) '())
(check-expect (replace (cons (make-phone 123 456 7890) '()))
              (cons (make-phone 123 456 7890) '()))
(check-expect (replace (cons (make-phone 713 123 4567) '()))
              (cons (make-phone 281 123 4567) '()))
(check-expect (replace (cons (make-phone 713 111 2222)
                             (cons (make-phone 832 333 4444)
                                   (cons (make-phone 713 555 6666) '()))))
              (cons (make-phone 281 111 2222)
                    (cons (make-phone 832 333 4444)
                          (cons (make-phone 281 555 6666) '()))))
(define (replace lop)
    (cond
        [(empty? lop) '()]
        [else (cons (replace-phone (first lop))
                    (replace (rest lop)))]))

; Phone -> Phone
; if phone area code is 713, replace it with 281,
; otherwise return original phone
(check-expect (replace-phone (make-phone 713 123 4567))
              (make-phone 281 123 4567))
(check-expect (replace-phone (make-phone 832 123 4567))
              (make-phone 832 123 4567))
(define (replace-phone p)
    (cond
        [(= (phone-area p) 713)
         (make-phone 281 (phone-switch p) (phone-four p))]
        [else p]))
