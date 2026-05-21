;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-196) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define LOCATION "/usr/share/dict/words")

(define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings:
; - "a"
; - ...
; - "z"
; or, equivalently, a member? of this list:
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

(define-struct letter-count [letter count])
; A LetterCount is a structure:
; (make-letter-count 1String Number)
; interpretation: (make-letter-count letter count)
; combines a letter and the count of occurrences for that letter

; A Dictionary is a List-of-Strings

; Dictionary -> List-of-letter-counts
; Counts how often each letter is used as the first one of word
; in the given dictionary
(check-expect (pick-letter-count "b"
                                 (count-by-letter
                                  (list "red" "yellow" "blue" "brown")))
              (make-letter-count "b" 2))
(define (count-by-letter dict)
  (count-by-letter-helper LETTERS dict))

; List-of-1strings Dictionary -> List-of-letter-counts
; Counts how often each letter from given letters is used as the first one
; of word in the given dictionary
(check-expect (count-by-letter-helper (list "b")
                                      (list "red" "yellow" "blue" "brown"))
              (list (make-letter-count "b" 2)))
(check-expect (count-by-letter-helper (list "a") '())
              (list (make-letter-count "a" 0)))
(define (count-by-letter-helper letters dict)
  (cond
    [(empty? letters) '()]
    [else (cons (make-letter-count (first letters)
                                   (starts-with# (first letters) dict))
                (count-by-letter-helper (rest letters) dict))]))

; 1String List-of-strings -> Number
; counts how many words in the given list start with the given letter
(check-expect (starts-with# "b" (list "yellow" "blue" "green" "brown")) 2)
(check-expect (starts-with# "z" '()) 0)
(define (starts-with# letter dict)
  (cond
    [(empty? dict) 0]
    [(starts-with? letter (first dict))
     (+ 1 (starts-with# letter (rest dict)))]
    [else (starts-with# letter (rest dict))]))

; 1String String -> Boolean
; determines whether given word starts with given letter
(check-expect (starts-with? "b" "blue") #true)
(check-expect (starts-with? "b" "yellow") #false)
(check-expect (starts-with? "b" "") #false)
(define (starts-with? letter word)
  (and (> (string-length word) 0)
       (string=? (string-ith word 0) letter)))

; 1String List-of-letter-counts -> LetterCount
; gets a LetterCount from the list for given letter
(check-expect (pick-letter-count "a" (list (make-letter-count "a" 13)
                                           (make-letter-count "b" 9)))
              (make-letter-count "a" 13))
(define (pick-letter-count letter lolc)
  (cond
    [(empty? lolc) (make-letter-count letter 0)]
    [(string=? (letter-count-letter (first lolc)) letter)
     (first lolc)]
    [else (pick-letter-count letter (rest lolc))]))

; (count-by-letter AS-LIST) ; uncomment to run on full dictionary
