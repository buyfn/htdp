;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-160) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; A Son.L is one of:
; - empty
; - (cons Number Son.L)
;
; Son is used when it
; applies to Son.L and Son.R

; Son
(define es '())

; Number Son.L -> SonL
; removes x from s
(define s1.L
  (cons 1 (cons 1 '())))

(check-expect (set-.L 1 s1.L) es)

(define (set-.L x s)
  (remove-all x s))

; Number Son.L -> Son.L
; adds x to s
(check-satisfied (set+.L 2 s1.L) is-member-2?)
(define (set+.L x s)
  (cons x s))

; A Son.R is one of:
; - empty
; - (cons Number Son.R)
;
; Constraint If s is a Son.R,
; no number occurs tice in s

; Number Son.R -> Son.R
; removes x from s
(define s1.R (cons 1 '()))

(check-expect (set-.R 1 s1.R) es)

(define (set-.R x s) (remove x s))

; Number Son.R -> SonR
; adds x to s
(check-satisfied (set+.R 2 s1.R) is-member-2?)
(define (set+.R x s)
  (if (in? x s)
      s
      (cons x s)))

; Number Son -> Boolean
; is x in s
(define (in? x s)
  (member? x s))

; Son -> Boolean
; #true if 2 is a member of s; #false otherwise
(define (is-member-2? s)
  (in? 2 s))