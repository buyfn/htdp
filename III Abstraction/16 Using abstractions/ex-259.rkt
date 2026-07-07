#lang htdp/isl
(require 2htdp/batch-io)

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

; Word -> List-of-words
; creates all rearrangements of the letters in w
(define (arrangements w)
  (local (; 1String List-of-words -> List-of-words
          ; creates a list of words with the given character inserted
          ; in all possible places for each word in the given list
          (define (insert-everywhere/in-all-words char low)
            (cond
              [(empty? low) '()]
              [else (append (insert-everywhere/word char (first low))
                            (insert-everywhere/in-all-words char (rest low)))]))
          ; 1String Word -> List-of-words
          ; creates a list of words by inserting given character
          ; in every position of the given word
          (define (insert-everywhere/word char word)
            (cond
              [(empty? word) (list (list char))]
              [else
               (cons (cons char word)
                     (prepend-char/for-each-word
                      (first word)
                      (insert-everywhere/word char (rest word))))]))
          ; 1String List-of-words -> List-of-words
          ; creates a list of words by attaching given character
          ; at the beginning of each word in the list
          (define (prepend-char/for-each-word char low)
            (cond
              [(empty? low) '()]
              [else (cons (cons char (first low))
                          (prepend-char/for-each-word char (rest low)))])))
    (cond
      [(empty? w) (list '())]
      [else
       (insert-everywhere/in-all-words
        (first w)
        (arrangements (rest w)))])))

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
    [else (cons (word->string (first low))
                (words->strings (rest low)))]))

; String -> Word
; converts s to the chosen word representation
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
