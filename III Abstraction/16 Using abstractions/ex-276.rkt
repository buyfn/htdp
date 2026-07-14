#lang htdp/isl+
(require 2htdp/itunes)

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

; String Date LTracks -> LTracks
; Selects tracks from the given album and that have been played
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
  (local (; Track -> Boolean
          ; determines whether given track was played after date
          (define (played-after-date? track)
            (date<? date (track-played track)))
          ; Track -> Boolean
          ; determines whether given track is from the album-title
          (define (same-album? track)
            (string=? (track-album track) album-title)))
    (filter played-after-date?
            (filter same-album? ltracks))))

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

; [List-of Track] -> [List-of [List-of Track]]
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
  (local (; String -> [List-of Track]
          ; returns a list of tracks from given album
          (define (select-tracks-from-album album)
            (local (; Track -> Boolean
                    ; determines whether given track is from the given album
                    (define (same-album? track)
                      (string=? (track-album track)
                                album)))
              (filter same-album? ltracks)))
          (define unique-albums
            (create-set (map track-album ltracks))))
    (map select-tracks-from-album unique-albums)))

; [List-of X] -> [List-of X]
; removes duplicates from l
(check-expect (create-set (list 1 2 3))
              (list 1 2 3))
(check-expect (create-set (list 1 1 1))
              (list 1))
(define (create-set l)
  (local (; X [List-of X] -> [List-of X]
          (define (next-acc current acc)
            (if (member? current acc)
                acc
                (cons current acc))))
    (foldr next-acc '() l)))
