#lang htdp/isl+

; String [List-of String] -> Boolean
; determines whether any name in lon is equal or
; an extension of name
(check-expect (find-name "Ann" '()) #false)
(check-expect (find-name "Ann" (list "Ann")) #true)
(check-expect (find-name "Ann" (list "Anna")) #true)
(check-expect (find-name "Ann" (list "Bob" "Carl")) #false)
(check-expect (find-name "Anna" (list "Ann")) #false)
(check-expect (find-name "Ann" (list "Joanna")) #false)
(define (find-name name lon)
  (local ((define name-length (string-length name)))
    (ormap
     (lambda (s)
       (cond
         [(< (string-length s) name-length) #false]
         [else (string=? (substring s 0 name-length) name)]))
     lon)))

; [List-of String] -> Boolean
; determines whether all strings in lon start with "a"
(check-expect (starts-with-a*? '()) #true)
(check-expect (starts-with-a*? (list "apple" "ant")) #true)
(check-expect (starts-with-a*? (list "apple" "bee")) #false)
(check-expect (starts-with-a*? (list "")) #false)
(check-expect (starts-with-a*? (list "Apple")) #false)
(define (starts-with-a*? lon)
  (andmap
   (lambda (s)
     (cond
       [(= (string-length s) 0) #false]
       [else (string=? (string-ith s 0) "a")]))
   lon))

; Either ormap or andmap could be used to implement
; a function that ensures that no name on some list exceeds a given width.
; Using andmap fits the problem statement better though.
