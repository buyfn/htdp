#lang htdp/isl+
(require 2htdp/abstraction)

(define-struct bsl-and [left right])
(define-struct bsl-or [left right])
(define-struct bsl-not [expr])

; A Bool-BSL-expr is one of:
; - Boolean
; - (make-bsl-and Bool-BSL-expr Bool-BSL-expr)
; - (make-bsl-or Bool-BSL-expr Bool-BSL-expr)
; - (make-bsl-not Bool-BSL-expr)
; interpretation: represents a Boolean BSL expression

; A Bool-BSL-value is a Boolean

; Bool-BSL-expr -> Bool-BSL-value
; evaluates a Bool-BSL-expr

; atoms
(check-expect (eval-bool-expression #true) #true)
(check-expect (eval-bool-expression #false) #false)

; and
(check-expect (eval-bool-expression (make-bsl-and #true #true)) #true)
(check-expect (eval-bool-expression (make-bsl-and #true #false)) #false)
(check-expect (eval-bool-expression (make-bsl-and #false #true)) #false)
(check-expect (eval-bool-expression (make-bsl-and #false #false)) #false)

; or
(check-expect (eval-bool-expression (make-bsl-or #true #true)) #true)
(check-expect (eval-bool-expression (make-bsl-or #true #false)) #true)
(check-expect (eval-bool-expression (make-bsl-or #false #true)) #true)
(check-expect (eval-bool-expression (make-bsl-or #false #false)) #false)

; not
(check-expect (eval-bool-expression (make-bsl-not #true)) #false)
(check-expect (eval-bool-expression (make-bsl-not #false)) #true)

; nested
(check-expect (eval-bool-expression (make-bsl-not (make-bsl-and #true #false)))
              #true)
(check-expect (eval-bool-expression (make-bsl-and (make-bsl-not #false)
                                                  (make-bsl-or #false #true)))
              #true)
(check-expect (eval-bool-expression (make-bsl-or (make-bsl-and #true #false)
                                                 (make-bsl-not #true)))
              #false)

(define (eval-bool-expression bool-bsl-expr)
  (match bool-bsl-expr
    [(? boolean?) bool-bsl-expr]
    [(bsl-and left right) (and (eval-bool-expression left) (eval-bool-expression right))]
    [(bsl-or left right) (or (eval-bool-expression left) (eval-bool-expression right))]
    [(bsl-not expr) (not (eval-bool-expression expr))]))
