;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-199) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))

(define date1 (create-date 2026 5 22 10 48 1))
(define date2 (create-date 2025 11 3 14 22 37))

(define track1 (create-track "One Of Your Girls"
                             "Troye Sivan"
                             "Something To Give Each Other"
                             181482
                             3
                             date1
                             4
                             date1))
(define track2 (create-track "Running Up That Hill"
                             "Kate Bush"
                             "Hounds of Love"
                             298000
                             12
                             date2
                             8
                             date2))
(define track3 (create-track "Rush"
                             "Troye Sivan"
                             "Something To Give Each Other"
                             194000
                             7
                             date1
                             5
                             date1))

(define ITUNES-LOCATION "Library.xml")

; LTracks
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))

; LTracks -> Number
; produces the total amount of play time
(check-expect (total-time (list track1 track2))
              (+ (track-time track1) (track-time track2)))
(define (total-time ltracks)
  (cond
    [(empty? ltracks) 0]
    [else (+ (track-time (first ltracks))
             (total-time (rest ltracks)))]))

; LTracks -> List-of-strings
; Consumes list of tracks and produces a list of album titles
(check-expect (select-all-album-titles
               (list track1 track2))
              (list (track-album track1)
                    (track-album track2)))
(define (select-all-album-titles ltracks)
  (cond
    [(empty? ltracks) '()]
    [else (cons (track-album (first ltracks))
                (select-all-album-titles (rest ltracks)))]))

; List-of-strings -> List-of-strings
; Constructs a list that contains every string from
; the input list exactly once

; empty list stays empty
(check-expect (create-set '()) '())
; single element is kept
(check-expect (create-set (list "a")) (list "a"))
; no duplicates – nothing to remove
(check-expect (create-set (list "a" "b")) (list "a" "b"))
; simple duplicate
(check-expect (create-set (list "a" "b" "a")) (list "b" "a"))
; all elements the same
(check-expect (create-set (list "x" "x" "x")) (list "x"))
; duplicate at the end
(check-expect (create-set (list "a" "b" "c" "c")) (list "a" "b" "c"))
; non-adjacent duplicates
(check-expect (create-set (list "a" "b" "a" "b")) (list "a" "b"))
; multiple distinct duplicates in a longer list
(check-expect (create-set (list "a" "b" "c" "b" "a" "d"))
              (list "c" "b" "a" "d"))
(define (create-set los)
  (cond
    [(empty? los) '()]
    [(member? (first los) (rest los))
     (create-set (rest los))]
    [else (cons (first los) (create-set (rest los)))]))

; LTracks -> List-of-strings
; Produces a list of unique album titles from the list of tracks
(check-expect (select-album-titles/unique
               (list track1 track2 track1))
              (list (track-album track2)
                    (track-album track1)))
(define (select-album-titles/unique ltracks)
  (create-set (select-all-album-titles ltracks)))

; String LTracks -> LTracks
; Extracts from the list of tracks those, which are from the
; specified album

; empty list produces empty
(check-expect (select-album "Hounds of Love" '()) '())
; single matching track
(check-expect (select-album "Hounds of Love" (list track2))
              (list track2))
; no tracks match
(check-expect (select-album "Nonexistent" (list track1 track2)) '())
; mixed: only matching tracks are kept
(check-expect (select-album "Hounds of Love" (list track1 track2))
              (list track2))
; all tracks match
(check-expect (select-album "Something To Give Each Other"
                             (list track1 track3))
              (list track1 track3))
; multiple matches among several tracks
(check-expect (select-album "Something To Give Each Other"
                             (list track1 track2 track3))
              (list track1 track3))
(define (select-album album-title ltracks)
  (cond
    [(empty? ltracks) '()]
    [(string=? (track-album (first ltracks)) album-title)
     (cons (first ltracks) (select-album album-title (rest ltracks)))]
    [else (select-album album-title (rest ltracks))]))

;; (total-time itunes-tracks)
;; (select-album-titles/unique itunes-tracks)
;; (length (select-album-titles/unique itunes-tracks))
