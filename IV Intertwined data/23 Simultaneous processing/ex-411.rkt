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

(define school-db
  (make-db school-schema school-content))

(define presence-schema
  `(,(make-spec "Present" boolean?)
    ,(make-spec "Description" string?)))

(define presence-content
  `((#true  "presence")
    (#false "absence")))

(define presence-schema-ext
  `(,(make-spec "Present" boolean?)
    ,(make-spec "Description" string?)
    ,(make-spec "Code" integer?)))

(define presence-content-ext
  `((#true  "presence" 1)
    (#false "absence"  0)))

(define presence-content-mul
  `((#true  "presence")
    (#true  "here")
    (#false "absence")
    (#false "there")))

(define presence-db
  (make-db presence-schema presence-content))

(define presence-db-ext
  (make-db presence-schema-ext
           presence-content-ext))

(define presence-db-mul
  (make-db presence-schema
           presence-content-mul))

(define joined-content
  `(("Alice" 35 "presence")
    ("Bob"   25 "absence")
    ("Carol" 30 "presence")
    ("Dave"  32 "absence")))

(define joined-content-ext
  `(("Alice" 35 "presence" 1)
    ("Bob"   25 "absence"  0)
    ("Carol" 30 "presence" 1)
    ("Dave"  32 "absence"  0)))

(define joined-content-mul
  `(("Alice" 35 "presence")
    ("Alice" 35 "here")
    ("Bob"   25 "absence")
    ("Bob"   25 "there")
    ("Carol" 30 "presence")
    ("Carol" 30 "here")
    ("Dave"  32 "absence")
    ("Dave"  32 "there")))

(define projected-content
  `(("Alice" #true)
    ("Bob"   #false)
    ("Carol" #true)
    ("Dave"  #false)))

; DB DB -> DB
; creates a new database by replacing the last cell in each row
; of db-1 with the translation of the cell in db-2
; Assumption: the schema of db-2 starts with the exact same Spec
; that the schema of db-1 ends in.
(check-expect (db-content (join school-db presence-db))
              joined-content)
(check-expect (map spec-label (db-schema (join school-db presence-db)))
              '("Name" "Age" "Description"))

; multiple columns in db-2
(check-expect (db-content (join school-db presence-db-ext))
              joined-content-ext)
(check-expect (map spec-label (db-schema (join school-db presence-db-ext)))
              '("Name" "Age" "Description" "Code"))

; multiple matching rows in db-2
(check-expect (db-content (join school-db presence-db-mul))
              joined-content-mul)
(check-expect (map spec-label (db-schema (join school-db presence-db-mul)))
              '("Name" "Age" "Description"))
(define (join db-1 db-2)
  (local ((define joined-schema
            (replace-last-item-with (rest (db-schema db-2))
                                    (db-schema db-1)))
          ; Any -> [List-of Row]
          (define (get-translations v)
            (select db-2
                    (map spec-label (rest (db-schema db-2)))
                    (lambda (row) (equal? (first row) v))))
          ; Row -> [List-of Row]
          (define (translate-row r)
            (map (lambda (t)
                   (replace-last-item-with t r))
                 (get-translations (last r))))
          (define joined-content
            (flat (map translate-row (db-content db-1)))))
    (make-db joined-schema
             joined-content)))

; [List-of [List-of X]] -> [List-of X]
; flattens the list
(define (flat l)
  (foldr append '() l))

; [X] [List-of X] -> X
; gets the last item of the list
; Assumption: the list is not empty
(check-expect (last '(1 2 3)) 3)
(define (last l) (first (reverse l)))

; [X] X [List-of X] -> [List-of X]
; replaces the last item of lst with item
; Assumption: the list is not empty
(check-expect (replace-last-item-with-item 99 '(1 2 3)) '(1 2 99))
(define (replace-last-item-with-item item lst)
  (reverse (cons item (rest (reverse lst)))))

; [X] [List-of X] [List-of X] -> [List-of X]
; replaces the last item of lst with a list of items
; Assumption: initial list is not empty
(check-expect (replace-last-item-with '(99) '(1 2 3)) '(1 2 99))
(check-expect (replace-last-item-with '(99 100) '(1 2 3)) '(1 2 99 100))
(define (replace-last-item-with replacement lst)
  (append (reverse (rest (reverse lst)))
          replacement))

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
