#lang htdp/isl+

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

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
(check-member-of (find-path 'A 'G sample-graph)
                 '(A B F G)
                 '(A B E F G)
                 '(A E F G))
(check-expect (find-path 'B 'C cyclic-graph)
              '(B E C))
(check-expect (find-path 'B 'H cyclic-graph) #false)
(check-expect (find-path 'B 'H sample-graph) #false)
(check-expect (find-path 'B 'A cyclic-graph) #false)
; When there are multiple paths, the function returns
; the one constructed by selecting the first neighbour
; from the list at every node
; (find-path 'A 'G sample-graph) => 
; (list 'A 'B 'E 'F 'G)
(define (find-path origination destination G)
  (local (; Node [List-of Node] -> [Maybe Path]
          ; finds a path from orig to destination in graph G
          ; keeps track of already visited nodes in seen,
          ; to stop infinite recurion in cyclic graphs
          (define (find-path/a orig seen)
            (cond
              [(symbol=? orig destination) (list destination)]
              [(member? orig seen) #false]
              [else (local ((define next (neighbors orig G))
                            (define candidate
                              (find-path/list next (cons orig seen))))
                      (cond
                        [(boolean? candidate) #false]
                        [else (cons orig candidate)]))]))
          ; [List-of Node] [List-of Node] -> [Maybe Path]
          ; finds a path from some node on lo-Os to destination
          ; if there is no path, the function produces #false
          (define (find-path/list lo-Os seen)
            (cond
              [(empty? lo-Os) #false]
              [else (local ((define candidate
                              (find-path/a (first lo-Os) seen)))
                      (cond
                        [(boolean? candidate)
                         (find-path/list (rest lo-Os) seen)]
                        [else candidate]))])))
    (find-path/a origination '())))
 
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
