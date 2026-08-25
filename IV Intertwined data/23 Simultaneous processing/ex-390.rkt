#lang htdp/isl+

(define INVALID_DIRECTIONS_ERROR "Invalid directions")

(define-struct branch [left right])
 
; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)

(define tos-0 'a)
(define tos-1 (make-branch 'a (make-branch 'b 'c)))
 
; A Direction is one of:
; – 'left
; – 'right
 
; A list of Directions is also called a path. 

; TOS [List-of Direction] -> TOS
; picks an item from the tree by following
; the list of directions
(check-expect (tree-pick tos-0 '()) 'a)
(check-expect (tree-pick tos-1 '(right left)) 'b)
(check-expect (tree-pick tos-1 '(right)) (make-branch 'b 'c))
(check-error (tree-pick tos-0 '(right right))
             INVALID_DIRECTIONS_ERROR)
(define (tree-pick tos lod)
  (cond
    [(and (empty? lod) (symbol? tos)) tos]
    [(and (empty? lod) (branch? tos)) tos]
    [(and (cons? lod) (symbol? tos))
     (error INVALID_DIRECTIONS_ERROR)]
    [(and (cons? lod) (branch? tos))
     (tree-pick ((if (symbol=? (first lod) 'left)
                     branch-left
                     branch-right) tos)
                (rest lod))]))
