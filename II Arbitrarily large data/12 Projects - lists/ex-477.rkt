#lang htdp/isl+

; [List-of X] -> [List-of [List-of X]]
; creates a list of all rearrangements of the items in w
; Trivial condition: input is an empty list
; Trivial answer: return a list containing an empty list.
; For a non-trivial argument, for every letter in the input,
; create arrangments for all letters except that one,
; then prepend that letter for each arrangement
; Solutions are combined into a single list with append
; Termination: terminates for all inputs, since at each recursive step,
; the input is reduced by one item, until it's a trivial case.
(define (arrangements w)
  (cond
    [(empty? w) '(())]
    [else
      (foldr (lambda (item others)
               (local ((define without-item
                         (arrangements (remove item w)))
                       (define add-item-to-front
                         (map (lambda (a) (cons item a))
                              without-item)))
                 (append add-item-to-front others)))
        '()
        w)]))

; arrangements and arrangements-old produce the same sets,
; but in a different order.
; arrangements prepends each letter of the input to the list
; of arrangements with that letter removed.
; arrangements-old takes each letter and inserts it in every
; position in all arrangements of the remaining letters
 
; [List-of [List-of 1String]] -> Boolean
; are the words "rat", "art", and "tar" members of the given list?
(define (all-words-from-rat? w)
  (and (member (explode "rat") w)
       (member (explode "art") w)
       (member (explode "tar") w)))
 
(check-satisfied (arrangements '("r" "a" "t"))
                 all-words-from-rat?)

; Word -> List-of-words
; creates all rearrangements of the letters in w
(define (arrangements-old w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w)
                                          (arrangements-old (rest w)))]))

; 1String List-of-words -> List-of-words
; creates a list of words with the given character inserted in all possible
; places for each word in the given list
; -- empty list of words
(check-expect (insert-everywhere/in-all-words "d" '()) '())

; -- single-letter word
(check-expect (insert-everywhere/in-all-words "d" (list (list "a")))
              (list (list "d" "a")
                    (list "a" "d")))

; -- two-letter word: "d" inserted into ("e" "r") at all positions
(check-expect (insert-everywhere/in-all-words "d" (list (list "e" "r")))
              (list (list "d" "e" "r")
                    (list "e" "d" "r")
                    (list "e" "r" "d")))

; -- multiple words in the list
(check-expect (insert-everywhere/in-all-words "d"
                (list (list "e" "r") (list "r" "e")))
              (list (list "d" "e" "r")
                    (list "e" "d" "r")
                    (list "e" "r" "d")
                    (list "d" "r" "e")
                    (list "r" "d" "e")
                    (list "r" "e" "d")))

; -- three-letter word: "a" inserted into ("b" "c" "d") at all positions
(check-expect (insert-everywhere/in-all-words "a"
                (list (list "b" "c" "d")))
              (list (list "a" "b" "c" "d")
                    (list "b" "a" "c" "d")
                    (list "b" "c" "a" "d")
                    (list "b" "c" "d" "a")))
(define (insert-everywhere/in-all-words char low)
  (cond
    [(empty? low) '()]
    [else (append (insert-everywhere/word char (first low))
                  (insert-everywhere/in-all-words char (rest low)))]))

; 1String Word -> List-of-words
; creates a list of words by inserting given character
; in every position of the given word
(check-expect (insert-everywhere/word "d" '())
              (list (list "d")))
(check-expect (insert-everywhere/word "d" (list "a"))
              (list (list "d" "a")
                    (list "a" "d")))
(check-expect (insert-everywhere/word "d" (list "a" "b"))
              (list (list "d" "a" "b")
                    (list "a" "d" "b")
                    (list "a" "b" "d")))
(define (insert-everywhere/word char word)
  (cond
    [(empty? word) (list (list char))]
    [else
     (cons (cons char word)
           (prepend-char/for-each-word (first word)
                                       (insert-everywhere/word char (rest word))))]))

; 1String List-of-words -> List-of-words
; creates a list of words by attaching given character
; at the beginning of each word in the list
(check-expect (prepend-char/for-each-word "d" '())
              '())
(check-expect (prepend-char/for-each-word "d" (list (list "a")))
              (list (list "d" "a")))
(check-expect (prepend-char/for-each-word "d" (list (list "a")
                                                    (list "b")))
              (list (list "d" "a")
                    (list "d" "b")))
(define (prepend-char/for-each-word char low)
  (cond
    [(empty? low) '()]
    [else (cons (cons char (first low))
                (prepend-char/for-each-word char (rest low)))]))
