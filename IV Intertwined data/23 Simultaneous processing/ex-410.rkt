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
  `(,(make-spec "Name" string?)
    ,(make-spec "Age" integer?)
    ,(make-spec "Present" boolean?)))

(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))

(define school-content-2
  `(("Alice" 35 #true)
    ("Sandy" 60 #true)
    ("Dick"  16 #false)))

(define school-content-3
  `(("Sandy" 60 #true)
    ("Dick"  16 #false)))

(define school-content-4
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)
    ("Sandy" 60 #true)
    ("Dick"  16 #false)))

(define school-db
  (make-db school-schema school-content))

(define school-db-2
  (make-db school-schema school-content-2))

(define school-db-3
  (make-db school-schema school-content-3))

(define school-db-4
  (make-db school-schema school-content-4))

; [X] [List-of X] [List-of X] -> [[List-of X] -> Boolean]
; specification for db-union
(define (union-of l1 l2)
  (lambda (r)
    (and (contains-all-from? l1 r)
         (contains-all-from? l2 r)
         (contains-all-from? r (append l1 l2))
         (no-duplicates? r))))

; [X] [List-of X] [List-of X] -> Boolean
; determines whether every element of l1 is a member of l2
(check-expect (contains-all-from? '(1 2) '(1 2 3)) #true)
(check-expect (contains-all-from? '(1 2) '(2 3)) #false)
(define (contains-all-from? l1 l2)
  (andmap (lambda (x) (member? x l2)) l1))

; [X] [List-of X] -> Boolean
; determines whether the are no duplicates in the list
(check-expect (no-duplicates? '(1 2 3)) #true)
(check-expect (no-duplicates? '(1 2 3 1)) #false)
(check-expect (no-duplicates? '()) #true)
(define (no-duplicates? l)
  (cond
    [(empty? l) #true]
    [(empty? (rest l)) #true]
    [(member? (first l) (rest l)) #false]
    [else (no-duplicates? (rest l))]))

; DB DB -> DB
; consumes two databases with same schemas,
; produces a new database with joint content of both
(check-satisfied (db-content (db-union school-db school-db-2))
                 (union-of (db-content school-db)
                           (db-content school-db-2)))
(define (db-union db-1 db-2)
  (local ((define content-1 (db-content db-1))
          (define content-2 (db-content db-2))
          ; Content Content -> Content
          (define (merge-content c-1 c-2)
            (create-set (append c-1 c-2))))
    (make-db (db-schema db-1)
             (merge-content content-1 content-2))))

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
