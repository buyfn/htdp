;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-209) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))
(define LOCATION "/usr/share/dict/words")
(define AS-LIST (read-lines LOCATION))

; A Word is a list of 1Strings

; A List-of-words is a list of Words

; String -> List-of-strings
; finds all words that the letters of some given word spell
(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))
(define (alternative-words s)
  (in-dictionary
   (words->strings (arrangements (string->word s)))))

; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary
(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [(in-dictionary/word? (first los) AS-LIST)
     (cons (first los) (in-dictionary (rest los)))]
    [else (in-dictionary (rest los))]))

; String List-of-strings -> Boolean 
(define (in-dictionary/word? word dict)
  (cond
    [(empty? dict) #false]
    [(string=? word (first dict)) #true]
    [else (in-dictionary/word? word (rest dict))]))

; Word -> List-of-words
; finds all rearrangements of word
(define (arrangements word)
  (list word))

; List-of-words -> List-of-strings
; converts a list of words into list of strings
(check-expect (words->strings '()) '())
(check-expect (words->strings (list (list "c" "a" "t")))
              (list "cat"))
(check-expect (words->strings (list (list "h" "i") (list "b" "y" "e")))
              (list "hi" "bye"))
(check-expect (words->strings (list (list "a") (list "b") (list "c")))
              (list "a" "b" "c"))
(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else (cons (implode (first low))
                (words->strings (rest low)))]))

; String -> Word
; converts s to the chohsen word representation
(check-expect (string->word "") '())
(check-expect (string->word "a") (list "a"))
(check-expect (string->word "hi") (list "h" "i"))
(check-expect (string->word "rat") (list "r" "a" "t"))
(check-expect (string->word "hello") (list "h" "e" "l" "l" "o"))
(check-expect (string->word "a b") (list "a" " " "b"))
(define (string->word s) (explode s))

; Word -> String
; converts w to a string
(check-expect (word->string '()) "")
(check-expect (word->string (list "a")) "a")
(check-expect (word->string (list "h" "i")) "hi")
(check-expect (word->string (list "r" "a" "t")) "rat")
(check-expect (word->string (list "h" "e" "l" "l" "o")) "hello")
(check-expect (word->string (list "a" " " "b")) "a b")
(define (word->string w) (implode w))
