;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-190) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))

; List-of-1Strings -> List-of-prefixes
; produces the list of all prefixes of list l
(check-expect (prefixes (list "a" "b" "c"))
              (list (list "a" "b" "c")
                    (list "a" "b")
                    (list "a")))
(check-expect (prefixes '()) '())
(define (prefixes l)
  (cond
    [(empty? l) '()]
    [else (cons l (prefixes (remove-last l)))]))

; List-of-1Strings -> List-of-suffixes
; produces the list of all suffixes of list l
(check-expect (suffixes '()) '())
(check-expect (suffixes (list "a" "b" "c"))
              (list (list "a" "b" "c")
                    (list "b" "c")
                    (list "c")))
(define (suffixes l)
  (cond
    [(empty? l) '()]
    [else (cons l (suffixes (rest l)))]))

; Prefix is a list of 1Strings
; A list p is a prefix of l if p and l
; are the same up through all items in p

; List -> List
; removes the last element of the list
(check-expect (remove-last (list "a")) '())
(check-expect (remove-last (list "a" "b")) (list "a"))
(define (remove-last l)
  (cond
    [(empty? (rest l)) '()]
    [else (cons (first l) (remove-last (rest l)))]))
