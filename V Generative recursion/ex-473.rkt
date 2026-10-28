#lang racket
(require 2htdp/abstraction)
(require test-engine/racket-tests)

; A Graph is a [List-of Connection]

; A Connection is a non-empty [List-of Node]
; where the first item is a Node
; and the rest of list are Nodes given Nodes connects to

; A Node is a Symbol

; A Path is a [List-of Node].
; interpretation The list of nodes specifies a sequence
; of immediate neighbors that leads from the first 
; Node on the list to the last one. 

(define sample-graph
  (list (list 'A 'B 'E)
        (list 'B 'E 'F)
        (list 'C 'D)
        (list 'D)
        (list 'E 'C 'F)
        (list 'F 'D 'G)
        (list 'G)))

(define cyclic-graph
  (list (list 'A 'B 'E)
        (list 'B 'E 'F)
        (list 'C 'B 'D)
        (list 'D)
        (list 'E 'C 'F)
        (list 'F 'D 'G)
        (list 'G)))

; Graph -> Boolean
; determines whether there is a path between every pair of nodes
(check-expect (test-on-all-nodes sample-graph) #false)
(check-expect (test-on-all-nodes (list (list 'A 'B)
                                       (list 'B))) #false)
(check-expect (test-on-all-nodes (list (list 'A 'B)
                                       (list 'B 'A))) #true)
; (test-on-all-nodes cyclic-graph) ; loops infinetely
(define (test-on-all-nodes g)
  (for/and ([p (all-pairs (graph->lo-nodes g))])
    (not (boolean? (find-path (first p) (second p) g)))))

; [X] [List-of [List-of X]] -> [List-of X]
; flattens a list of nested lists
(define (flat l)
  (foldl append '() l))

; [X] [List-of X] -> [List-of (list X X)]
; returns a list of all possible pairs
; of the elements of l
(define (all-pairs l)
  (local (; [X] X [List-of X] -> [List-of (list X X)]
          (define (pairs-with-each elt l)
            (flat (map (lambda (x) (list (list elt x)
                                         (list x elt))) l))))
    (cond
      [(< (length l) 2) '()]
      [(= (length l) 2) (list l (reverse l))]
      [else (append (pairs-with-each (first l) (rest l))
                    (all-pairs (rest l)))])))

; Graph -> [List-of Node]
; produces a list of Nodes in the given Graph g
(check-expect (graph->lo-nodes sample-graph)
              '(A B C D E F G))
(define (graph->lo-nodes g)
  (map connection-node g))

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
(check-member-of (find-path 'A 'G sample-graph)
                 '(A B F G)
                 '(A B E F G)
                 '(A E F G))
(check-expect (find-path 'B 'C cyclic-graph)
              '(B E C))
; When there are multiple paths, the function returns
; the one constructed by selecting the first neighbour
; from the list at every no
; (find-path 'A 'G sample-graph) => 
; (list 'A 'B 'E 'F 'G)
(define (find-path origination destination G)
  (local (; [List-of Node] -> [Maybe Path]
          ; finds a path from some node on lo-Os to destination
          ; if there is no path, the function produces #false
          (define (find-path/list lo-Os)
            (ormap (lambda (n) (find-path n destination G)) lo-Os)))
    (cond
      [(symbol=? origination destination) (list destination)]
      [else (local ((define next (neighbors origination G))
                    (define candidate
                      (find-path/list next)))
              (cond
                [(boolean? candidate) #false]
                [else (cons origination candidate)]))])))
 
; Node Graph -> [List-of Node]
; returns immediate neighbors of a given node in graph.
; Signals an error if the node is not in the graph.
(check-expect (neighbors 'A sample-graph) '(B E))
(define (neighbors node graph)
  (cond
    [(empty? graph) (error "Node not in graph")]
    [(symbol=? (connection-node (first graph)) node)
     (connection-neighbors (first graph))]
    [else (neighbors node (rest graph))]))

; Connection -> Node
; returns a Node of the given Connection
(define connection-node first)

; Connection -> [List-of Node]
; returns a list of connected Nodes for given Connection
(define connection-neighbors rest)

(test)
