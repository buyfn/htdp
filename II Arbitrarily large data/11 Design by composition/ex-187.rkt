;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-187) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))

(define-struct gp [name score])
; A GamePlayer is a structure:
; (make-gp String Number)
; interpretation (make-gp p s) represents player p who
; scored a maximum of s points

; List-of-gameplayers -> List-of-gameplayers
; sorts the list of GamePlayers by score in descending order
(check-expect (sort-by-score '()) '())
(check-expect (sort-by-score (list (make-gp "A" 10))) (list (make-gp "A" 10)))
(check-expect (sort-by-score (list (make-gp "A" 10)
                                   (make-gp "B" 20)
                                   (make-gp "C" 15)))
              (list (make-gp "B" 20) (make-gp "C" 15) (make-gp "A" 10)))
(check-expect (sort-by-score (list (make-gp "A" 10)
                                   (make-gp "B" 20)
                                   (make-gp "C" 10)))
              (list (make-gp "B" 20) (make-gp "A" 10) (make-gp "C" 10)))

(define (sort-by-score logp)
  (cond
    [(empty? logp) '()]
    [else (insert-gp (first logp)
                     (sort-by-score (rest logp)))]))

; GamePlayer List-of-gameplayers -> List-of-gameplayers
; Inserts a GamePlayer p into a sorted list of GamePlayers logp
(check-expect (insert-gp (make-gp "X" 15) '()) (list (make-gp "X" 15)))
(check-expect (insert-gp (make-gp "X" 15) (list (make-gp "A" 20) (make-gp "B" 10)))
              (list (make-gp "A" 20) (make-gp "X" 15) (make-gp "B" 10)))
(check-expect (insert-gp (make-gp "X" 25) (list (make-gp "A" 20) (make-gp "B" 10)))
              (list (make-gp "X" 25) (make-gp "A" 20) (make-gp "B" 10)))
(check-expect (insert-gp (make-gp "X" 5) (list (make-gp "A" 20) (make-gp "B" 10)))
              (list (make-gp "A" 20) (make-gp "B" 10) (make-gp "X" 5)))
(check-expect (insert-gp (make-gp "X" 20) (list (make-gp "A" 20) (make-gp "B" 10)))
              (list (make-gp "X" 20) (make-gp "A" 20) (make-gp "B" 10)))
(define (insert-gp p logp)
  (cond
    [(empty? logp) (list p)]
    [else (if (gte p (first logp))
              (cons p logp)
              (cons (first logp)
                    (insert-gp p (rest logp))))]))

; GamePlayer GamePlayer -> Boolean
; returns true if score of first player
; is greater or equal to score of the second player
(check-expect (gte (make-gp "A" 20) (make-gp "B" 10)) #true)
(check-expect (gte (make-gp "A" 10) (make-gp "B" 20)) #false)
(check-expect (gte (make-gp "A" 10) (make-gp "B" 10)) #true)

(define (gte p1 p2)
  (>= (gp-score p1) (gp-score p2)))
