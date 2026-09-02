#lang htdp/isl+

(define NO_ITEM_BEYOND_PATTERN_ERROR "No item found in search beyond pattern")

; [List-of Symbol] [List-of Symbol] -> Boolean
; returns #true if the pattern is identical to the
; initial part of the search string
(check-expect (DNAprefix '(a) '(a c g)) #true)
(check-expect (DNAprefix '(a c g) '(a c g)) #true)
(check-expect (DNAprefix '(t) '(a c g)) #false)
(define (DNAprefix pattern search)
  (cond
    [(empty? pattern) #true]
    [(empty? search) #false]
    [(not (symbol=? (first pattern) (first search))) #false]
    [else (DNAprefix (rest pattern) (rest search))]))

; [List-of Symbol] [List-of Symbol] -> [Maybe Symbol]
; returns the first item in the search string beyond pattern
; if the pattern doesn't match the beginning of the search, returns #false
; if pattern and search are identical, signals an error
(check-expect (DNAdelta '(a) '(a c g)) 'c)
(check-error (DNAdelta '(a c) '(a c)) NO_ITEM_BEYOND_PATTERN_ERROR)
(check-expect (DNAdelta '(a c g) '(a c)) #false)
(check-expect (DNAdelta '(t) '(a c g)) #false)
(define (DNAdelta pattern search)
  (cond
    [(and (empty? pattern) (empty? search)) (error NO_ITEM_BEYOND_PATTERN_ERROR)]
    [(and (empty? pattern) (cons? search)) (first search)]
    [(and (cons? pattern) (empty? search)) #false]
    [(not (symbol=? (first pattern) (first search))) #false]
    [else (DNAdelta (rest pattern) (rest search))]))
