#lang htdp/isl+
(require 2htdp/abstraction)

(define-struct db [schema content])
; A DB is a structure: (make-db Schema Content)
 
; A Schema is a [List-of Spec]

(define-struct spec [label predicate])
; Spec is a structure: (make-spec Label Predicate)
; A Label is a String
; A Predicate is a [Any -> Boolean]
 
; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions 
 
; integrity constraint In (make-db sch con), 
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch

(define school-schema
  `(("Name"    ,string?)
    ("Age"     ,integer?)
    ("Present" ,boolean?)))

(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))

(define school-db
  (make-db school-schema school-content))

(define presence-schema
  `(("Present"     ,boolean?)
    ("Description" ,string?)))

(define presence-content
  `((#true  "presence")
    (#false "absence")))

(define presence-db
  (make-db presence-schema presence-content))

(define presence-content-invalid
  `((#true "presence")
    (#false "absence")
    (0 "also absence")))

(define presence-db-invalid
  (make-db presence-schema presence-content-invalid))

; DB -> Boolean
; do all rows in db satisfy (I1) and (I2)
(check-expect (integrity-check school-db) #true)
(check-expect (integrity-check presence-db) #true)
(check-expect (integrity-check presence-db-invalid) #false)
(define (integrity-check db)
  (local (; Row -> Boolean
          ; does row satisfy (I1) and (I2)
          (define (row-integrity-check row)
            (and (= (length row) (length (db-schema db)))
                 (andmap2 (lambda (s c) ((second s) c))
                          (db-schema db)
                          row))))
    (andmap row-integrity-check (db-content db))))

; [X Y] [X Y -> Boolean] [List-of X] [List-of Y] -> Boolean
; Applies f to the pairs of corresponding values from both lists,
; and returns #true if f returns #true for every pair,
; otherwise, returns #false
(check-expect (andmap2 (lambda (x y) (and (string? x) (number? y)))
                       '(1 2 3)
                       '("a" "b" "c"))
              #false)
(check-expect (andmap2 (lambda (x y) (and (string? x) (number? y)))
                       '("a" "b" "c")
                       '(1 2 3))
              #true)
(check-expect (andmap2 (lambda (x y) (and (string? x) (number? y)))
                       '("a" "b" "c")
                       '(1 2 "d"))
              #false)
(define (andmap2 f l1 l2)
  (for/and ((x l1) (y l2))
    (f x y)))
