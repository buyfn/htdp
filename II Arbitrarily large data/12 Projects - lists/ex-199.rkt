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

(total-time itunes-tracks)

