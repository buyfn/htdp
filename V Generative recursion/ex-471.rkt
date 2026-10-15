#lang htdp/isl+

; A Graph is a [List-of Connection]

; A Connection is a non-empty [List-of Node]
; where the first item is a Node
; and the rest of list are Nodes given Nodes connects to

; A Node is a Symbol

(define sample-graph
  (list (list 'A 'B 'E)
        (list 'B 'E 'F)
        (list 'C 'D)
        (list 'D)
        (list 'E 'C 'F)
        (list 'F 'D 'G)
        (list 'G)))

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
