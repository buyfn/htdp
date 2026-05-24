;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex-205) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))

(define ITUNES-LOCATION "Library.xml")

; LLists
(define list-tracks
  (read-itunes-as-lists ITUNES-LOCATION))

(define date1 (create-date 2026 5 22 10 48 1))
(define date2 (create-date 2025 11 3 14 22 37))

; LAssoc
(define track1 (list (list "name" "One Of Your Girls")
                     (list "artist" "Troye Sivan")
                     (list "album" "Something To Give Each Other")
                     (list "Total Time" 181482)
                     (list "id" 3)
                     (list "added" date1)
                     (list "play#" 4)
                     (list "played" date2)))

(define track2 (list (list "name" "Running Up That Hill")
                     (list "artist" "Kate Bush")
                     (list "album" "Hounds of Love")
                     (list "Total Time" 298000)
                     (list "id" 12)
                     (list "added" date2)
                     (list "play#" 8)
                     (list "played" date2)))

;; tracks with boolean attributes for testing
(define track3 (list (list "name" "Bohemian Rhapsody")
                     (list "Purchased" #true)
                     (list "Total Time" 354000)))

(define track4 (list (list "name" "Stairway to Heaven")
                     (list "Purchased" #true)
                     (list "Movie" #false)
                     (list "Total Time" 482000)))

(define track-list (list track1 track2))

; String LAssoc Any -> Association
; Consumes key, LAssoc and a default value.
; Produces the first Association whose first item is equal to key,
; or default if there is no such Association.

; key is the first association
(check-expect (find-association "name" track1 #false)
              (list "name" "One Of Your Girls"))
; key is in the middle
(check-expect (find-association "album" track1 #false)
              (list "album" "Something To Give Each Other"))
; key is not found → returns default
(check-expect (find-association "missing" track1 #false) #false)
; empty LAssoc → returns default
(check-expect (find-association "name" '() #false) #false)
(define (find-association key lassoc default)
  (cond
    [(empty? lassoc) default]
    [(string=? (first (first lassoc)) key) (first lassoc)]
    [else (find-association key (rest lassoc) default)]))

; LLists -> Number
; Produces the total amount of play time for all tracks in the list

; empty list
(check-expect (total-time/list '()) 0)
; single track
(check-expect (total-time/list (list track1)) 181482)
; two tracks
(check-expect (total-time/list (list track1 track2)) (+ 181482 298000))
(define (total-time/list track-list)
  (cond
    [(empty? track-list) 0]
    [else (+ (second (find-association "Total Time" (first track-list) 0))
             (total-time/list (rest track-list)))]))

; LAssoc -> List-of-strings
; Produces the strings that are associated with a Boolean attribute in LAssoc

; no boolean attributes
(check-expect (boolean-attributes/lassoc track1) '())
; one boolean attribute
(check-expect (boolean-attributes/lassoc track3) (list "Purchased"))
; two boolean attributes
(check-expect (boolean-attributes/lassoc track4) (list "Purchased" "Movie"))
; empty LAssoc
(check-expect (boolean-attributes/lassoc '()) '())

; LLists -> List-of-strings
; Produces the strings that are associated with a Boolean attribute in LLists

; empty list of tracks
(check-expect (boolean-attributes '()) '())
; tracks without booleans
(check-expect (boolean-attributes (list track1 track2)) '())
; one track with one boolean
(check-expect (boolean-attributes (list track3)) (list "Purchased"))
; two tracks, shared boolean key → deduplicated
(check-expect (boolean-attributes (list track3 track4))
              (list "Purchased" "Movie"))
; mix of tracks with and without booleans
(check-expect (boolean-attributes (list track1 track4))
              (list "Purchased" "Movie"))
(define (boolean-attributes llists)
  (cond
    [(empty? llists) '()]
    [else (create-set (append (boolean-attributes/lassoc (first llists))
                              (boolean-attributes (rest llists))))]))

; LAssoc -> List-of-strings
; Produces the strings that are associated with a Boolean attribute in LAssoc
(define (boolean-attributes/lassoc lassoc)
  (cond
    [(empty? lassoc) '()]
    [(boolean? (second (first lassoc)))
     (create-set (cons (first (first lassoc))
                       (boolean-attributes/lassoc (rest lassoc))))]
    [else (boolean-attributes/lassoc (rest lassoc))]))

; List-of-strings -> List-of-strings
; Constructs a list that contains every string from
; the input list exactly once
(define (create-set los)
  (cond
    [(empty? los) '()]
    [(member? (first los) (rest los))
     (create-set (rest los))]
    [else (cons (first los) (create-set (rest los)))]))

;; (total-time/list list-tracks)
;; (boolean-attributes list-tracks)
