;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-163) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

; List-of-strings -> List-of-strings
; consumes a list of toy descriptions and replaces
; all occurences of "robot" with "r2d2"
(check-expect (subst-robot '()) '())
(check-expect (subst-robot (cons "robot" '())) (cons "r2d2" '()))
(check-expect (subst-robot (cons "bear" (cons "robot" (cons "doll" '()))))
              (cons "bear" (cons "r2d2" (cons "doll" '()))))
(define (subst-robot lot)
    (cond
        [(empty? lot) '()]
        [(string=? (first lot) "robot")
         (cons "r2d2" (subst-robot (rest lot)))]
        [else (cons (first lot) (subst-robot (rest lot)))]))

; String String List-of-strings -> List-of-strings
; replaces `old` with `new` in `los`
; (check-expect (substitute "a" "b" '()) '())
(check-expect (substitute "a" "b" (cons "a" '())) (cons "b" '()))
(check-expect (substitute "a" "b" (cons "c" (cons "a" (cons "d" '())))) (cons "c" (cons "b" (cons "d" '()))))
(check-expect (substitute "a" "b" (cons "c" (cons "d" '()))) (cons "c" (cons "d" '())))
(check-expect (substitute "a" "b" (cons "a" (cons "c" (cons "a" '())))) (cons "b" (cons "c" (cons "b" '()))))
(define (substitute old new los)
    (cond
        [(empty? los) '()]
        [(string=? (first los) old)
         (cons new (substitute old new (rest los)))]
        [else (cons (first los)
              (substitute old new (rest los)))]))
