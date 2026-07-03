;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-234) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp") (lib "web-io.rkt" "teachpack" "2htdp")) #f)))

; A RankedSong is a list
; (list Number String)

(define one-list
  '("Asia: Heat of the moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

; List-of-strings -> ... nested list ...
(define (make-ranking los)
  `(html
    (head
     (title "Top songs"))
    (body
     (table ((border "1")) ,@(make-table (ranking los))))))

; List-of-strings -> ... nested list ...
; creates an HTML table from list
(define (make-table los)
  (cond
    [(empty? los) '()]
    [else `((tr ((width "300"))
                ,@(make-row (first los)))
            ,@(make-table (rest los)))]))

; List-of-ranked-songs -> ... nested list ...
; creates a row for an HTML table from l
(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l))
                (make-row (rest l)))]))

; Number or String -> ... nested list ...
; creates a cell for an HTML table from a number
(define (make-cell n)
  `(td ,(if (number? n)
            (number->string n)
            n)))

; List-of-strings -> List-of-ranked-songs
; Adds a rank to every song in list based on its position in los
(define (ranking los)
  (reverse (add-ranks (reverse los))))

; List-of-strings -> List-of-ranked-songs
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))

(show-in-browser (make-ranking one-list))
