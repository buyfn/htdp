#lang htdp/isl+
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

; A Letter is one of the following 1Strings:
; - "a"
; - ...
; - "z"
; or, equivalently, a member? of this list:
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

(define LOCATION "/usr/share/dict/words")
(define AS-LIST (read-lines LOCATION))
(define SIZE (length AS-LIST))

; An HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed

; HM-Word N -> String
; runs a simplistic hangman game, produces the current state
(define (play the-pick time-limit)
  (local ((define the-word (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
          ; HM-Word  KeyEvent -> HM-Word
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
               [to-draw render-word]
               [on-tick do-nothing 1 time-limit]
               [on-key checked-compare]))))

; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))

; HM-Word HM-Word KeyEvent -> HM-Word
(check-expect (compare-word '("d" "o" "g") '("_" "_" "_") "d") '("d" "_" "_"))
(check-expect (compare-word '("d" "o" "g") '("_" "_" "_") "c") '("_" "_" "_"))
(check-expect (compare-word '("d" "o" "g") '("d" "o" "_") "g") '("d" "o" "g"))
(define (compare-word the-word current-status ke)
  (cond
    [(empty? the-word) current-status]
    [else (cons (first (if (string=? (first the-word) ke) the-word current-status))
                (compare-word (rest the-word) (rest current-status) ke))]))

(play (list-ref AS-LIST (random SIZE)) 60)
