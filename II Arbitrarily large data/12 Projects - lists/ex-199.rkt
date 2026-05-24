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

; String Date LTracks -> LTracks
; Selects tracks from the given album and have been played
; after the given date

; empty list produces empty
(check-expect (select-album-date "Hounds of Love" date1 '()) '())
; matching album, played after date -> keep
(check-expect (select-album-date "Something To Give Each Other"
                                  date2
                                  (list track1))
              (list track1))
; matching album, played before date -> drop
(check-expect (select-album-date "Hounds of Love"
                                  date1
                                  (list track2))
              '())
; non-matching album -> drop regardless of date
(check-expect (select-album-date "Nonexistent" date2 (list track1)) '())
; mixed: only tracks with matching album AND played after date are kept
(check-expect (select-album-date "Something To Give Each Other"
                                  date2
                                  (list track1 track2 track3))
              (list track1 track3))
; all tracks match album but only some played after date
(check-expect (select-album-date "Something To Give Each Other"
                                  date1
                                  (list track1 track3))
              '())
(define (select-album-date album-title date ltracks)
  (select-date date (select-album album-title ltracks)))

; Date LTracks -> LTracks
; selects tracks that had been played after the given date
(define (select-date date ltracks)
  (cond
    [(empty? ltracks) '()]
    [(date<? date (track-played (first ltracks)))
     (cons (first ltracks) (select-date date (rest ltracks)))]
    [else (select-date date (rest ltracks))]))

; Date Date -> Boolean
; returns whether the first date occurs before the second one

; earlier year
(check-expect (date<? (create-date 2024 1 1 0 0 0)
                      (create-date 2025 1 1 0 0 0))
              #true)
; later year
(check-expect (date<? (create-date 2026 1 1 0 0 0)
                      (create-date 2025 1 1 0 0 0))
              #false)
; same year, earlier month
(check-expect (date<? (create-date 2025 3 1 0 0 0)
                      (create-date 2025 11 1 0 0 0))
              #true)
; same year, later month
(check-expect (date<? (create-date 2025 11 1 0 0 0)
                      (create-date 2025 3 1 0 0 0))
              #false)
; same year+month, earlier day
(check-expect (date<? (create-date 2025 5 2 0 0 0)
                      (create-date 2025 5 22 0 0 0))
              #true)
; same year+month, later day
(check-expect (date<? (create-date 2025 5 22 0 0 0)
                      (create-date 2025 5 2 0 0 0))
              #false)
; same date, earlier hour
(check-expect (date<? (create-date 2025 5 22 8 0 0)
                      (create-date 2025 5 22 14 0 0))
              #true)
; same date+hour, earlier minute
(check-expect (date<? (create-date 2025 5 22 10 15 0)
                      (create-date 2025 5 22 10 48 0))
              #true)
; same date+hour+minute, earlier second
(check-expect (date<? (create-date 2025 5 22 10 48 0)
                      (create-date 2025 5 22 10 48 59))
              #true)
; completely equal dates
(check-expect (date<? (create-date 2025 5 22 10 48 1)
                      (create-date 2025 5 22 10 48 1))
              #false)
; using the pre-defined dates: date2 (2025-11-03) is before date1 (2026-05-22)
(check-expect (date<? date2 date1) #true)
(check-expect (date<? date1 date2) #false)
(define (date<? d1 d2)
  (cond
    [(< (date-year d1) (date-year d2)) #true]
    [(and (= (date-year d1) (date-year d2))
          (< (date-month d1) (date-month d2)))
     #true]
    [(and (= (date-year d1) (date-year d2))
          (= (date-month d1) (date-month d2))
          (< (date-day d1) (date-day d2)))
     #true]
    [(and (= (date-year d1) (date-year d2))
          (= (date-month d1) (date-month d2))
          (= (date-day d1) (date-day d2))
          (< (date-hour d1) (date-hour d2)))
     #true]
    [(and (= (date-year d1) (date-year d2))
          (= (date-month d1) (date-month d2))
          (= (date-day d1) (date-day d2))
          (= (date-hour d1) (date-hour d2))
          (< (date-minute d1) (date-minute d2)))
     #true]
    [(and (= (date-year d1) (date-year d2))
          (= (date-month d1) (date-month d2))
          (= (date-day d1) (date-day d2))
          (= (date-hour d1) (date-hour d2))
          (= (date-minute d1) (date-minute d2))
          (< (date-second d1) (date-second d2)))
     #true]
    [else #false]))

; LTracks -> List-of-LTracks
; groups tracks by albums

; empty list produces empty
(check-expect (select-albums '()) '())
; single track -> one group with that track
(check-expect (select-albums (list track2))
              (list (list track2)))
; two tracks, different albums -> two groups, one track each
(check-expect (select-albums (list track1 track2))
              (list (list track1) (list track2)))
; three tracks, two albums -> two groups;
; create-set keeps last occurrence so "Hounds of Love" comes first
(check-expect (select-albums (list track1 track2 track3))
              (list (list track2)
                    (list track1 track3)))
; all tracks from the same album -> one group
(check-expect (select-albums (list track1 track3))
              (list (list track1 track3)))
(define (select-albums ltracks)
  (group-by-album (select-album-titles/unique ltracks) ltracks))

; List-of-strings LTracks -> List-of-LTracks
; groups tracks by albums from provided list
(define (group-by-album album-titles ltracks)
  (cond
    [(empty? album-titles) '()]
    [else (cons (select-album (first album-titles) ltracks)
                (group-by-album (rest album-titles) ltracks))]))

;; (total-time itunes-tracks)
;; (select-album-titles/unique itunes-tracks)
;; (length (select-album-titles/unique itunes-tracks))
