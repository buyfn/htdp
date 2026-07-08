#lang htdp/isl

; N -> [List-of [List-of Number]]
; Creates identity matrix of size s
(check-expect (identityM 0) '())
(check-expect (identityM 1) (list (list 1)))
(check-expect (identityM 2) (list (list 1 0)
                                  (list 0 1)))
(check-expect (identityM 3) (list (list 1 0 0)
                                  (list 0 1 0)
                                  (list 0 0 1)))
(define (identityM s)
  (cond
    [(zero? s) '()]
    [else
     (local (; [List-of Number] -> [List-of Number]
             ; prepends 0 to l
             (define (prepend-zero l) (cons 0 l))
             ; [List-of [List-of Number]] -> [List-of [List-of Number]]
             ; prepends 0 for every list in rows
             (define (prepend-zero* rows)
               (map prepend-zero rows))
             ; Number -> Number
             ; returns 1 if col-index = 0
             ; 0 otherwise
             (define (first-row-entry col-index)
               (if (zero? col-index) 1 0))
             (define first-row
               (build-list s first-row-entry)))
       (cons first-row
             (prepend-zero* (identityM (- s 1)))))]))
