;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-187) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))

(define-struct email [from date message])
; An EmailMessage is a structure
; (make-email String Number String)
; interpretation (make-email f d m) represents text m
; sent by f, d seconds after the beginning of time

(define email1 (make-email "a@a.com" 100 "message 1"))
(define email2 (make-email "b@b.com" 200 "message 2"))
(define email3 (make-email "c@c.com" 300 "message 3"))

; List-of-emails -> List-of-emails
; sorts the list of EmailMessages by date
(define (sort-by-date loe)
  (cond
    [(empty? loe) '()]
    [else (email-insert-by-date (first loe)
                                (sort-by-date (rest loe)))]))

; List-of-emails -> List-of-emails
; sorts the list of EmailMessages by name
(check-expect (sort-by-name '()) '())
(check-expect (sort-by-name (list email1)) (list email1))
(check-expect (sort-by-name (list email3 email2 email1))
              (list email1 email2 email3))

(define (sort-by-name loe)
  (cond
    [(empty? loe) '()]
    [else (email-insert-by-name (first loe)
                                (sort-by-name (rest loe)))]))

; EmailMessage List-of-emails -> List-of-emails
; Inserts an EmailMessage e into a sorted by date list of EmailMessages loe
(check-expect (sort-by-date '()) '())
(check-expect (sort-by-date (list email1)) (list email1))
(check-expect (sort-by-date (list email3 email1 email2))
              (list email1 email2 email3))

(define (email-insert-by-date e loe)
  (cond
    [(empty? loe) (list e)]
    [else (if (<= (email-date e) (email-date (first loe)))
              (cons e loe)
              (cons (first loe)
                    (email-insert-by-date e (rest loe))))]))

; EmailMessage List-of-emails -> List-of-emails
; Inserts an EmailMessage e into a sorted by name list of EmailMessages loe
(check-expect (email-insert-by-name email1 '()) (list email1))
(check-expect (email-insert-by-name email2 (list email1 email3))
              (list email1 email2 email3))

(define (email-insert-by-name e loe)
  (cond
    [(empty? loe) (list e)]
    [else (if (string<? (email-from e) (email-from (first loe)))
              (cons e loe)
              (cons (first loe)
                    (email-insert-by-name e (rest loe))))]))

