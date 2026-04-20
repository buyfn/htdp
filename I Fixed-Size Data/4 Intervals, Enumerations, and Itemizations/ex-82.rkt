;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-82) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; A Letter is either 1String of #false

; A Word is a struct
; (make-word [Letter Letter Letter])
(define-struct word [first second third])

; Word Word -> Word
; Compares two word and produces a word
; preserves the original property content at positions where two words match
; otherwise places #false

; all match
(check-expect (compare-word (make-word "a" "b" "c")
                            (make-word "a" "b" "c"))
              (make-word "a" "b" "c"))
; 1st and 2nd match, 3rd differs
(check-expect (compare-word (make-word "a" "b" "c")
                             (make-word "a" "b" "d"))
              (make-word "a" "b" #false))
; 1st and 3rd match, 2nd differs
(check-expect (compare-word (make-word "a" "x" "c")
                            (make-word "a" "b" "c"))
              (make-word "a" #false "c"))
; only 1st match
(check-expect (compare-word (make-word "a" "x" "z")
                            (make-word "a" "b" "c"))
              (make-word "a" #false #false))
; 2nd and 3rd match
(check-expect (compare-word (make-word "x" "b" "c")
                            (make-word "a" "b" "c"))
              (make-word #false "b" "c"))
; none match
(check-expect (compare-word (make-word "x" "y" "z")
                            (make-word "a" "b" "c"))
              (make-word #false #false #false))

(define (compare-word w1 w2)
  (make-word
   (if (string=? (word-first w1) (word-first w2)) (word-first w1) #false)
   (if (string=? (word-second w1) (word-second w2)) (word-second w1) #false)
   (if (string=? (word-third w1) (word-third w2)) (word-third w1) #false)))