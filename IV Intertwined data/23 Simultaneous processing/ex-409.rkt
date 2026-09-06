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

(define school-schema-reordered
  `(,(make-spec "Present" boolean?)
    ,(make-spec "Age" integer?)
    ,(make-spec "Name" string?)))

(define school-content-reordered
  `((#true  35 "Alice")
    (#false 25 "Bob")
    (#true  30 "Carol")
    (#false 32 "Dave")))

(define school-db
  (make-db school-schema school-content))

(define presence-schema
  `(,(make-spec "Present" boolean?)
    ,(make-spec "Description" string?)))

(define presence-content
  `((#true  "presence")
    (#false "absence")))

(define presence-db
  (make-db presence-schema presence-content))

(define projected-content
  `(("Alice" #true)
    ("Bob"   #false)
    ("Carol" #true)
    ("Dave"  #false)))

; DB [List-of Label] -> DB
; produces a database like input-db, but with its columns
; reordered according to lol.
; Omits unrequested columns, ignores unknown ones.
(check-expect (db-content (reorder school-db '("Present" "Age" "Name")))
              school-content-reordered)
(check-expect (db-content (reorder school-db '("Present" "Occupation" "Age" "Name")))
              school-content-reordered)
(check-expect (db-content (reorder school-db '("Age" "Name")))
              `((35 "Alice")
                (25 "Bob")
                (30 "Carol")
                (32 "Dave")))
(define (reorder input-db lol)
  (local ((define init-labels (map spec-label (db-schema input-db)))
          (define filtered-labels
            (filter (lambda (l) (member? l init-labels)) lol))
          ; Label -> N
          ; finds index of label in the original db
          (define (get-index l)
            (find-index l init-labels))
          (define index-mask
            (map get-index filtered-labels))
          ; Schema -> Schema
          ; reorder schema according to lol
          (define (reorder-schema s)
            (map (lambda (i) (list-ref s i)) index-mask))
          ; Content -> Content
          ; reorders content according to lol
          (define (reorder-content c)
            (map reorder-row c))
          ; Row -> Row
          ; reorders items in Row according to lol
          (define (reorder-row r)
            (map (lambda (i) (list-ref r i)) index-mask))
          (define reordered-content (reorder-content (db-content input-db)))
          (define reordered-schema (reorder-schema (db-schema input-db))))
    (make-db reordered-schema reordered-content)))

; DB [List-of Label] [Row -> Boolean] -> [List-of Row]
; returns a list of rows that satisfy the given predicate,
; projected down to the given set of labels
(check-expect (select school-db
                      '("Name")
                      (lambda (row) (too-old? (second row))))
              '(("Alice")
                ("Dave")))
(define (select db labels p)
  (db-content
   (project
    (make-db (db-schema db)
             (filter p (db-content db)))
    labels)))

; DB [List-of Label] -> DB
; retains a column from DB if its label is in labels
(check-expect (db-content (project school-db '("Name" "Present")))
              projected-content)
(define (project db labels)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          
          ; Spec -> Boolean
          ; does this spec belong to the new schema
          (define (keep? c)
            (member? (spec-label c) labels))

          (define mask (map keep? schema))
          
          ; Row -> Row
          ; retains those columns whose name is in labels
          (define (row-project row)
            (foldr (lambda (cell m c)
                     (if m
                         (cons cell c)
                         c))
                   '()
                   row
                   mask)))
    (make-db (filter keep? schema)
             (map row-project content))))

; Number -> Boolean
(define (too-old? age) (> age 30))

; String [List-of String] -> N
; finds the index of String s in the List l
; Assumption: target strings exists in l
(check-expect (find-index "a" '("a" "b" "c")) 0)
(check-expect (find-index "c" '("a" "b" "c")) 2)
(define (find-index s l)
  (cond
    [(string=? (first l) s) 0]
    [else (+ 1 (find-index s (rest l)))]))
