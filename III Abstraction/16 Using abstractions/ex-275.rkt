#lang htdp/isl+

(define-struct letter-count [letter count])
; A LetterCount is a structure:
; (make-letter-count 1String Number)
; interpretation: (make-letter-count letter count)
; combines a letter and the count of occurrences for that letter

; A Dictionary is a [List-of String]

; Dictionary -> LetterCount
; Produces the LetterCount for the letter that occurs most often
; as the first one in the given Dictionary
(check-expect (most-frequent (list "red" "yellow" "blue" "brown"))
              (make-letter-count "b" 2))
(define (most-frequent dict)
  (local (; Dictionary -> LetterCount
          ; creates letter count where the letter is the first letter of all
          ; items in dict and count is the length of the dict
          (define (create-letter-count dict)
            (make-letter-count (first-letter (first dict))
                               (length dict)))
          ; LetterCount LetterCount -> Boolean
          ; determines whether the first letter-count is larger than the second
          (define (letter-count>? lc1 lc2)
            (> (letter-count-count lc1)
               (letter-count-count lc2))))
    (first (sort (map create-letter-count (words-by-first-letter dict))
                 letter-count>?))))

; Dictionary -> [List-of Dictionary]
; produces a list of dictionaries, one per letter,
; from the original dictionary
(check-expect (words-by-first-letter (list "red" "green" "blue" "brown"))
              (list (list "red")
                    (list "green")
                    (list "blue" "brown")))
(define (words-by-first-letter dict)
  (local (; 1String -> Dictionary
          ; selects words from dict that start with given letter
          (define (words-starting-with letter)
            (local (; String -> Boolean
                    (define (starts-with-letter? word)
                      (string=? (first-letter word) letter)))
              (filter starts-with-letter? dict)))
          (define first-letters
            (create-set (map first-letter dict))))
    (map words-starting-with first-letters)))

; String -> 1String
; returns the first letter of a word
(define (first-letter str) (string-ith str 0))

; [List-of X] -> [List-of X]
; removes duplicates from l
(check-expect (create-set (list 1 2 3))
              (list 1 2 3))
(check-expect (create-set (list 1 1 1))
              (list 1))
(define (create-set l)
  (local (; X [List-of X] -> [List-of X]
          (define (next-acc current acc)
            (if (member? current acc)
                acc
                (cons current acc))))
    (foldr next-acc '() l)))
