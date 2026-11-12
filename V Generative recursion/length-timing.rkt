#lang htdp/isl+

(define (searchL x l)
  (cond
    [(empty? l) #false]
    [else
     (or (= (first l) x)
         (searchL x (rest l)))]))

(define (searchS x l)
  (cond
    [(= (length l) 0) #false]
    [else
     (or (= (first l) x)
         (searchS x (rest l)))]))

; N -> [List Number Number]
; how long do searchS and searchL take
; to look for n in (list 0 ... (- n 1))
(define (timing n)
  (local ((define long-list
            (build-list n (lambda (x) x))))
    (list
     (time (searchS n long-list))
     (time (searchL n long-list)))))
;; cpu time: 260 real time: 271 gc time: 0
;; cpu time: 3 real time: 4 gc time: 0
