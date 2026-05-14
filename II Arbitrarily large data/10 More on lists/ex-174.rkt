;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-172) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)

; A List-of-strings is one of:
; - '()
; - (cons String List-of-strings)

; A LLS is one of:
; - '()
; - (cons List-of-strings LLS)

; String -> String?
(define (encode n)
  (write-file (string-append "encoded-" n)
              (encode-lines (read-words/line n))))

; LLS -> String
(define (encode-lines lls)
  (cond
    [(empty? lls) ""]
    [(empty? (rest lls)) (encode-line (first lls))]
    [else (string-append (encode-line (first lls))
                         "\n"
                         (encode-lines (rest lls)))]))

; List-of-strings -> String
; encodes a line of text
(check-expect (encode-line '()) "")
(check-expect (encode-line (cons "hi" (cons "dude" '())))
              "104105 100117100101")
(define (encode-line los)
  (cond
    [(empty? los) ""]
    [(empty? (rest los)) (encode-word (explode (first los)))]
    [else (string-append (encode-word (explode (first los)))
                         " "
                         (encode-line (rest los)))]))

; List-of-1strings -> String
; encodes a word represented as a list of 1Strings
(check-expect (encode-word '()) "")
(check-expect (encode-word (cons "h" (cons "i" '()))) "104105")
(define (encode-word los)
  (cond
    [(empty? los) ""]
    [else (string-append (encode-letter (first los))
                         (encode-word (rest los)))]))

; 1String -> String
; converts the given 1String to a 3-letter numeric String
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10) (string-append "00" (code1 s))]
    [(< (string->int s) 100) (string-append "0" (code1 s))]))

; 1String -> String
; converts the given 1String into a String
(check-expect (code1 "z") "122")
(define (code1 c)
  (number->string (string->int c)))

(encode "ttt.txt")
