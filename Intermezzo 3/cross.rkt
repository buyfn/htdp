#lang htdp/isl+
(require 2htdp/abstraction)

; [X] [List-of X] [List-of X] -> [List-of [List-of X]]
; produces pairs of all items from l1 and l2
(check-expect (cross empty (list 1 2)) empty)
(check-expect (cross (list 1 2) empty) empty)
(check-satisfied (cross (list 'a 'b 'c) (list 1 2))
                  (lambda (result)
                    (and (= (length result) 6)
                         (andmap (lambda (p) (member? p result))
                                 (list (list 'a 1) (list 'a 2)
                                       (list 'b 1) (list 'b 2)
                                       (list 'c 1) (list 'c 2))))))

(define (cross l1 l2)
  (for*/list ([first l1] [second l2])
    (list first second)))
