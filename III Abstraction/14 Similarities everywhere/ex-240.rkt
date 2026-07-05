;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex-240) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

(define-struct layer [stuff])

; An LStr [L String]
(define lstr-1 "hello")
(define lstr-2 (make-layer "hello"))
(define lstr-3 (make-layer (make-layer "hello")))

; An LNum is [L Number]
; - Number
; - (make-layer LNum)
(define lnum-1 1)

; An [L T] is one of:
; - CT
; - (make-layer [L T])
