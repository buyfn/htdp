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

; Dictionary -> LetterCount
; Produces the LetterCount for the letter that occurs most often
; as the first one in the given Dictionary
(check-expect (most-frequent (list "red" "yellow" "blue" "brown"))
              (make-letter-count "b" 2))
(define (most-frequent dict)
  (most-frequent-letter-count (count-by-letter dict)))

; Dictionary -> LetterCount
; Produces the LetterCount for the letter that occurs most often
; as the first one in the given Dictionary
; by sorting all LetterCounts and selecting the first
(check-expect (most-frequent.v2 (list "red" "yellow" "blue" "brown"))
              (make-letter-count "b" 2))
(define (most-frequent.v2 dict)
  (first
   (letter-count-sort
    (count-by-letter dict))))

; Dictionary -> LetterCount
; Produces the LetterCount for the letter that occurs most often
; as the first one in the given Dictionary
; by producing a list of dictionaries by letter
(check-expect (most-frequent.v3 (list "red" "yellow" "blue" "brown"))
              (make-letter-count "b" 2))
(define (most-frequent.v3 dict)
  (most-frequent-letter-count
   (dictionaries->letter-counts
    (words-by-first-letter dict))))

; List-of-dictionaries -> List-of-letter-counts
(define (dictionaries->letter-counts dicts)
  (cond
    [(empty? dicts) '()]
    [(empty? (first dicts)) (dictionaries->letter-counts (rest dicts))]
    [else (cons (dictionary->letter-count (first dicts))
                (dictionaries->letter-counts (rest dicts)))]))

; Dictionary -> LetterCount
; Produces a LetterCount from a Dictionary that
; contains words starting from the same letter
(check-expect (dictionary->letter-count (list "brown" "blue"))
              (make-letter-count "b" 2))
(define (dictionary->letter-count dict)
  (make-letter-count (string-ith (first dict) 0)
                     (length dict)))

; Dictionary -> List-of-dictionaries
; produces a list of dictionaries, one per letter,
; from the original dictionary
(define (words-by-first-letter dict)
  (words-by-first-letter-aux LETTERS dict))

; List-of-letters Dictionary -> List-of-dictionaries
(define (words-by-first-letter-aux letters dict)
  (cond
    [(empty? letters) '()]
    [else (cons (words-starting-with-letter (first letters) dict)
                (words-by-first-letter-aux (rest letters) dict))]))

; 1String Dictionary -> Dictionary
; Returns a list of words that start with the given letter
(define (words-starting-with-letter letter dict)
  (cond
    [(empty? dict) '()]
    [(starts-with? letter (first dict))
     (cons (first dict) (words-starting-with-letter letter (rest dict)))]
    [else (words-starting-with-letter letter (rest dict))]))

; A MaybeLetterCount is one of:
; - LetterCount
; - #false

; List-of-letter-counts -> MaybeLetterCount
; gets most frequent LetterCount from the list
(check-expect (most-frequent-letter-count
               (list (make-letter-count "a" 1)
                     (make-letter-count "b" 2)))
              (make-letter-count "b" 2))
(define (most-frequent-letter-count lolc)
  (cond
    [(empty? lolc) #false]
    [(empty? (rest lolc)) (first lolc)]
    [else (more-frequent (first lolc)
                         (most-frequent-letter-count (rest lolc)))]))

; List-of-letter-counts -> List-of-letter-counts
; Sorts the list of LetterCounts in descending order
; empty list
(check-expect (letter-count-sort '()) '())
; single element
(check-expect (letter-count-sort (list (make-letter-count "a" 3)))
              (list (make-letter-count "a" 3)))
; already in descending order
(check-expect (letter-count-sort (list (make-letter-count "a" 5)
                                       (make-letter-count "b" 3)
                                       (make-letter-count "c" 1)))
              (list (make-letter-count "a" 5)
                    (make-letter-count "b" 3)
                    (make-letter-count "c" 1)))
; ascending order — needs full reversal
(check-expect (letter-count-sort (list (make-letter-count "c" 1)
                                       (make-letter-count "b" 3)
                                       (make-letter-count "a" 5)))
              (list (make-letter-count "a" 5)
                    (make-letter-count "b" 3)
                    (make-letter-count "c" 1)))
; mixed order
(check-expect (letter-count-sort (list (make-letter-count "b" 3)
                                       (make-letter-count "a" 5)
                                       (make-letter-count "c" 1)))
              (list (make-letter-count "a" 5)
                    (make-letter-count "b" 3)
                    (make-letter-count "c" 1)))
(define (letter-count-sort lolc)
  (cond
    [(empty? lolc) '()]
    [(empty? (rest lolc)) lolc]
    [else (insert (first lolc) (letter-count-sort (rest lolc)))]))

; LetterCount List-of-letter-counts -> List-of-letter-counts
; inserts a new LetterCount into a sorted list into the correct place
; into empty list
(check-expect (insert (make-letter-count "a" 3) '())
              (list (make-letter-count "a" 3)))
; at front — larger than all existing
(check-expect (insert (make-letter-count "a" 10)
                      (list (make-letter-count "b" 5)
                            (make-letter-count "c" 2)))
              (list (make-letter-count "a" 10)
                    (make-letter-count "b" 5)
                    (make-letter-count "c" 2)))
; at end — smaller than all existing
(check-expect (insert (make-letter-count "d" 1)
                      (list (make-letter-count "b" 5)
                            (make-letter-count "c" 2)))
              (list (make-letter-count "b" 5)
                    (make-letter-count "c" 2)
                    (make-letter-count "d" 1)))
; in the middle
(check-expect (insert (make-letter-count "x" 3)
                      (list (make-letter-count "a" 5)
                            (make-letter-count "c" 1)))
              (list (make-letter-count "a" 5)
                    (make-letter-count "x" 3)
                    (make-letter-count "c" 1)))
; equal count — inserted after existing with same count
(check-expect (insert (make-letter-count "z" 5)
                      (list (make-letter-count "a" 5)
                            (make-letter-count "c" 1)))
              (list (make-letter-count "a" 5)
                    (make-letter-count "z" 5)
                    (make-letter-count "c" 1)))
(define (insert lc lolc)
  (cond
    [(empty? lolc) (cons lc lolc)]
    [(letter-count>? lc (first lolc))
     (cons lc lolc)]
    [else (cons (first lolc)
                (insert lc (rest lolc)))]))

; LetterCount LetterCount -> Boolean
; Returns true if first letter is more frequent than second
; second is more frequent
(check-expect (letter-count>? (make-letter-count "a" 1)
                              (make-letter-count "b" 2))
              #false)
; first is more frequent
(check-expect (letter-count>? (make-letter-count "x" 5)
                              (make-letter-count "y" 3))
              #true)
; equal counts
(check-expect (letter-count>? (make-letter-count "a" 4)
                              (make-letter-count "b" 4))
              #false)
; both zero
(check-expect (letter-count>? (make-letter-count "c" 0)
                              (make-letter-count "d" 0))
              #false)
; large counts
(check-expect (letter-count>? (make-letter-count "m" 100)
                              (make-letter-count "n" 99))
              #true)
(define (letter-count>? lca lcb)
  (> (letter-count-count lca)
     (letter-count-count lcb)))

; LetterCount LetterCount -> LetterCount
; selects more frequent LetterCount between two options
(check-expect (more-frequent (make-letter-count "a" 1)
                             (make-letter-count "b" 2))
              (make-letter-count "b" 2))
; first argument has higher count
(check-expect (more-frequent (make-letter-count "x" 5)
                             (make-letter-count "y" 3))
              (make-letter-count "x" 5))
; equal counts — returns second
(check-expect (more-frequent (make-letter-count "a" 4)
                             (make-letter-count "b" 4))
              (make-letter-count "b" 4))
; both zero — returns second
(check-expect (more-frequent (make-letter-count "c" 0)
                             (make-letter-count "d" 0))
              (make-letter-count "d" 0))
; large counts
(check-expect (more-frequent (make-letter-count "m" 100)
                             (make-letter-count "n" 99))
              (make-letter-count "m" 100))
(define (more-frequent lca lcb)
  (if (letter-count>? lca lcb) lca lcb))

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

; uncomment to run on full dictionary
; (count-by-letter AS-LIST)
; (most-frequent AS-LIST)
; (most-frequent.v2 AS-LIST)
; (most-frequent.v3 AS-LIST)

;; (check-expect (most-frequent AS-LIST)
;;              (most-frequent.v2 AS-LIST))
;; (check-expect (most-frequent AS-LIST)
;;               (most-frequent.v3 AS-LIST))
