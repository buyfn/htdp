#lang htdp/isl+
	
(define (special P)
  (cond
    [(empty? P) (solve P)]
    [else
     (combine-solutions
       P
       (special (rest P)))]))

; Compute the lengths of input

;; ; '() -> 0
;; (define (solve P) 0)

;; ; [List-of Any] -> Number
;; (define (combine-solutions s-1 s-2)
;;   (+ 1 s-2))

; Negate each number in the list

;; ; '() -> '()
;; (define (solve P) '())

;; ; [List-of Number] -> [List-of Number]
;; (define (combine-solutions s-1 s-2)
;;   (cons (* (first s-1) -1) s-2))

;; ; Uppercase every string in the list

;; ; '() -> '()
;; (define (solve P) '())

;; ; [List-of String] -> [List-of String]
;; (define (combine-solutions s-1 s-2)
;;   (cons (string-upcase (first s-1)) s-2))
