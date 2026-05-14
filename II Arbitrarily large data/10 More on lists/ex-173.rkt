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
; reads the file with provided name n, removes the articles,
; writes the result out to a file whose name is the result of
; concatenating "no-articles-" with n
; An article is one of the following: "a", "an", "the"
(define (remove-articles n)
  (write-file (string-append "no-aritcles-" n)
              (collapse (remove-articles-lls (read-words/line n)))))

; LLS -> LLS
; removes articles from LLS
(check-expect (remove-articles-lls '()) '())
(check-expect (remove-articles-lls (list (list "hello" "world"))) (list (list "hello" "world")))
(check-expect (remove-articles-lls (list (list "a" "dog") (list "an" "apple"))) (list (list "dog") (list "apple")))
(check-expect (remove-articles-lls (list (list "the") (list "quick" "brown" "fox"))) (list '() (list "quick" "brown" "fox")))
(define (remove-articles-lls lls)
  (cond
    [(empty? lls) '()]
    [else (cons (remove-articles-list (first lls))
                (remove-articles-lls (rest lls)))]))

; List-of-stirngs -> List-of-strings
; removes articles from the list of strings
(check-expect (remove-articles-list '()) '())
(check-expect (remove-articles-list (list "hello" "world")) (list "hello" "world"))
(check-expect (remove-articles-list (list "a" "an" "the")) '())
(check-expect (remove-articles-list (list "a" "boy" "and" "an" "apple")) (list "boy" "and" "apple"))
(define (remove-articles-list los)
  (cond
    [(empty? los) '()]
    [(article? (first los)) (remove-articles-list (rest los))]
    [else (cons (first los) (remove-articles-list (rest los)))]))

; String -> Boolean
; determines whether given string is an article
; An article is one of the following: "a", "an", "the"
(check-expect (article? "a") #true)
(check-expect (article? "an") #true)
(check-expect (article? "the") #true)
(check-expect (article? "hello") #false)
(define (article? s)
  (member? s (list "a" "an" "the")))

; LLS -> String
; converts a list of lines into a string
(check-expect (collapse '()) "")
(check-expect (collapse (list (list "hello" "world") (list "bye"))) "hello world\nbye")
(check-expect (collapse (list (list "a" "b") (list "c" "d"))) "a b\nc d")
(check-expect (collapse (list (list "onlyone"))) "onlyone")
(define (collapse lls)
    (cond
        [(empty? lls) ""]
        [(empty? (rest lls)) (collapse-line (first lls))]
        [else (string-append (collapse-line (first lls)) "\n"
                             (collapse (rest lls)))]))

; List-of-strings -> String
; converts a list of strings into a string
(check-expect (collapse-line '()) "")
(check-expect (collapse-line (list "hello" "world")) "hello world")
(check-expect (collapse-line (list "a" "b" "c")) "a b c")
(check-expect (collapse-line (list "single")) "single")
(define (collapse-line los)
    (cond
        [(empty? los) ""]
        [(empty? (rest los)) (first los)]
        [else (string-append (first los) " " (collapse-line (rest los)))]))

(remove-articles "ttt.txt")
