#lang htdp/isl+

(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; A Token is one of:
; - 1String
; - String, consisting of lower-case letters only

; Line -> [List-of Token]
; Turns a Line into a list of tokens
(check-expect (tokenize (list "h" "o" "w" " " "a" "r" "e" " " "y" "o" "u"))
              (list "how" "are" "you"))
(check-expect (tokenize (list "a" "!" "b"))
              (list "a" "!" "b"))
(check-expect (tokenize (list "H" "i"))
              (list "H" "i"))
(check-expect (tokenize (list "!" "?"))
              (list "!" "?"))
(define (tokenize line)
  (local (; String [List-of Token] -> [List-of Token]
          (define (append-word w l)
            (if (= 0 (string-length w))
                l
                (append l (list w))))
          ; String [List-of Token] Line -> [List-of Token]
          (define (iter cur acc rest-of-line)
            (cond
              [(empty? rest-of-line) (append-word cur acc)]
              [(string-whitespace? (first rest-of-line))
               (iter "" (append-word cur acc) (rest rest-of-line))]
              [(not (member? (first rest-of-line) LETTERS))
               (iter ""
                     (append-word (first rest-of-line) (append-word cur acc))
                     (rest rest-of-line))]
              [else
               (iter (string-append cur (first rest-of-line))
                     acc
                     (rest rest-of-line))])))
    (iter "" '() line)))
