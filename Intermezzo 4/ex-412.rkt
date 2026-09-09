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
