#lang htdp/isl+

(define-struct inex [mantissa sign exponent])
; An Inex is a structure:
; (make-inex N99 S N99)
; An S is one of:
; - 1
; - -1
; An N99 is an N between 0 and 99 (inclusive)

; N Number N -> Inex
; makes an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and (<= 0 m 99)
          (<= 0 e 99)
          (or (= s 1) (= s -1)))
     (make-inex m s e)]
    [else (error "bad value given")]))

; Inex -> Number
; converts an inex into its numeric equivalent
(define (inex->number an-inex)
  (* (inex-mantissa an-inex)
     (expt 10 (* (inex-sign an-inex)
                 (inex-exponent an-inex)))))

(define MAX-POSITIVE (create-inex 99 1 99))
(define MIN-POSITIVE (create-inex 1 -1 99))

; Inex Inex -> Inex
; multiples two Inex numbers together
; signals an error, if the result is out of range
(check-expect (inex* (create-inex 2 1 4)
                     (create-inex 8 1 10))
              (create-inex 16 1 14))
(check-expect (inex* (create-inex 20 1 1)
                     (create-inex 5 1 4))
              (create-inex 10 1 6))
(check-expect (inex* (create-inex 27 -1 1)
                     (create-inex 7 1 4))
              (create-inex 19 1 4))
(check-expect (inex* (create-inex 2 -1 2)
                     (create-inex 3 1 0))
              (create-inex 6 -1 2))
;; (check-expect (inex* (create-inex 1 1 99)
;;                      (create-inex 1 1 1))
;;               (create-inex 10 1 99))
(define (inex* a b)
  (local ((define mantissa-product
            (* (inex-mantissa a)
               (inex-mantissa b)))
          (define mantissa-normalisation-result
            (normalize-mantissa mantissa-product))
          (define mantissa-normalized (first mantissa-normalisation-result))
          (define new-signed-exponent
            (+ (second mantissa-normalisation-result)
               (+ (* (inex-sign a) (inex-exponent a))
                  (* (inex-sign b) (inex-exponent b)))))
          (define new-sign
            (if (>= new-signed-exponent 0) 1 -1))
          (define new-exponent (abs new-signed-exponent)))
    (if (> new-exponent 99)
        (error "product out of bounds")
        (create-inex mantissa-normalized new-sign new-exponent))))

; Inex Inex -> Inex
; adds two Inex numbers together
; signals an error, if the result is out of range
(check-expect (inex+ (create-inex 55 1 0)
                     (create-inex 55 1 0))
              (create-inex 11 1 1))
(check-expect (inex+ (create-inex 55 -1 0)
                     (create-inex 55 -1 0))
              (create-inex 11 1 1))
(check-expect (inex+ (create-inex 56 1 0)
                     (create-inex 56 1 0))
              (create-inex 11 1 1))
(check-expect (inex+ (create-inex 55 -1 1)
                     (create-inex 55 -1 1))
              (create-inex 11 1 0))
(define (inex+ a b)
  (local ((define mantissa-sum
            (+ (inex-mantissa a)
               (inex-mantissa b)))
          (define mantissa-overflow? (> mantissa-sum 99))
          (define mantissa-sum-normalized
            (if mantissa-overflow?
                (round (/ mantissa-sum 10))
                mantissa-sum))
          (define signed-exponent
            (* (inex-sign a) (inex-exponent a)))
          (define new-signed-exponent
            (if mantissa-overflow?
                (+ signed-exponent 1)
                signed-exponent))
          (define new-sign
            (if (>= new-signed-exponent 0) 1 -1))
          (define new-exponent (abs new-signed-exponent)))
    (if (> new-exponent 99)
        (error "sum out of bounds")
        (create-inex mantissa-sum-normalized
                     new-sign
                     new-exponent))))

; N -> N
; how many digits given number has
(check-expect (num-digits 1) 1)
(check-expect (num-digits 12) 2)
(check-expect (num-digits 123) 3)
(define (num-digits n)
  (if (< n 10)
      1
      (+ 1 (num-digits (quotient n 10)))))

; N -> [List N N]
; divides a number by 10 and rounds it untill it's smaller than 100
; returnes normalized value and the number of times the initial number
; was divided by 10
(check-expect (normalize-mantissa 10) (list 10 0))
(check-expect (normalize-mantissa 100) (list 10 1))
(check-expect (normalize-mantissa 99) (list 99 0))
(check-expect (normalize-mantissa 999) (list 10 2))
(check-expect (normalize-mantissa 1234) (list 12 2))
(define (normalize-mantissa n)
  (local (; [List N N] -> [List N N]
          (define (iter res)
            (if (< (first res) 100)
                res
                (local ((define divide-times
                          (- (num-digits (first res)) 2))
                        (define next-n
                          (round (/ (first res) (expt 10 divide-times)))))
                  (iter (list next-n (+ divide-times (second res))))))))
    (iter (list n 0))))
