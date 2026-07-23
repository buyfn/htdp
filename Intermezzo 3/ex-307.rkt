#lang htdp/isl+
(require 2htdp/abstraction)

; String [List-of String] -> [Maybe String]
; returns first name in lon that is equal or
; an extension of name
(check-expect (find-name "Ann" '()) #false)
(check-expect (find-name "Ann" (list "Ann")) "Ann")
(check-expect (find-name "Ann" (list "Anna")) "Anna")
(check-expect (find-name "Ann" (list "Bob" "Carl")) #false)
(check-expect (find-name "Anna" (list "Ann")) #false)
(check-expect (find-name "Ann" (list "Joanna")) #false)
(define (find-name name lon)
  (local ((define name-length (string-length name))
          ; String -> Boolean
          ; determines whether s is equal or extension of name
          (define (equal-or-extension? s)
            (cond
              [(< (string-length s) name-length) #false]
              [else (string=? (substring s 0 name-length) name)])))
    (for/or ([n lon])
      (if (equal-or-extension? n) n #false))))

; N [List-of String] -> Boolean
; ensures that no name in lon is longer than width
(check-expect (no-longer-than? 3 (list "Ann" "Bob" "Mary")) #false)
(check-expect (no-longer-than? 3 (list "Ann" "Bob")) #true)
(define (no-longer-than? width lon)
  (for/and ([n lon])
    (<= (string-length n) width)))
