#lang racket

(require data/gvector)

;;;
;Graph Primitives
;;;

;Graph Constructor.  Nothing interesting, since the primitives to mutate the graph are defined below
(define (make-graph) (make-gvector))

;Graph constructor that returns a graph with each vertex specified in vertices (list)
(define (make-graph-with-vertices vertices)
  (let ((graph (make-gvector)))
    (map (lambda (n) (make-vertex graph n)) vertices)
    graph))

;Graph constructor that returns a graph with each edge (and vertex) specified in edges (list).  
;Each element of edges is a list whose car is the source of the edge, 
;and whose cadr is the destination of the edge.
(define (make-graph-with-edges edges)
  (let ((graph (make-gvector)))
    (map (lambda (n) (make-edge graph (car n) (cadr n))) edges)
    graph))

;Full constructor for mathematical definition of a graph, G = (V, E)
;Works exactly the same as the two constructors above.
(define (make-graph-vertices-and-edges vertices edges)
  (let ((graph (make-gvector)))
    (map (lambda (n) (make-vertex graph n)) vertices)
    (map (lambda (n) (make-edge graph (car n) (cadr n))) edges)
    graph))
  

;Higher order procedure that gets either the incoming or outgoing edges (based upon the value of index) and returns them in list form
(define (get-edges graph vertex-name index)
  (if (not (equal? (get-vertex graph vertex-name) -1))
      (gvector->list (gvector-ref (gvector-ref graph (get-vertex graph vertex-name)) index))
      (begin
        (display "Error: The vertex provided is not in the graph")
        (newline))))

;Gets all edges leaving a vertex
(define (get-outgoing-edges graph vertex-name)
  (get-edges graph vertex-name 1))

;Gets all edges entering a vertex
(define (get-incoming-edges graph vertex-name)
  (get-edges graph vertex-name 2))

;Return true if a graph includes the vertex in question, otherwise false
(define (graph-include? graph vertex-name)
  (define new-name vertex-name)
  (cond 
    [(symbol? vertex-name) (set! new-name (symbol->string vertex-name))])
  (if (> (length (filter 
                  (lambda (vertex) 
                    (equal? vertex new-name)) 
                  (map 
                   (lambda (n) 
                     (if (symbol? (gvector-ref n 0))
                         (symbol->string (gvector-ref n 0))
                         (gvector-ref n 0))) 
                   (gvector->list graph)))) 0)
    #t
    #f))

;Prints the given graph
(define (show-graph graph)
  
  ;Helper procedure, prints information pertaining to a single vertex
  (define print-element (lambda (n)
                          (begin
                            (display (gvector-ref n 0)) 
                            (display ":\t")
                            (display "Outgoing Edges: ")
                            (display (gvector->list (gvector-ref n 1)))
                            (display "\t")
                            (display "Incoming Edges: ")
                            (display (gvector->list (gvector-ref n 2))))))
                            
  ;Helper procedure, iterates through graph calling print-element
  (define (iter i)
    (if (>= i (gvector-count graph))
      (newline)
      (begin
        (print-element (gvector-ref graph i))
        (newline)
        (iter (+ i 1)))))
  (iter 0))

;Helper procedure that returns the index of a string in a list (if it is a member), otherwise -1.
(define (index-of name vertices [start 0])
      (if (equal? vertices '())
          -1
          (if (equal? (car vertices) name)
              start
              (index-of name (cdr vertices) (+ start 1)))))

;Gets the index of the vertex #{vertex-name} in the graph
(define (get-vertex graph vertex-name)
  (define new-name vertex-name)
  (cond 
    [(symbol? vertex-name) (set! new-name (symbol->string vertex-name))])
  
  (index-of new-name (map 
            (lambda (n) 
              (if (symbol? (gvector-ref n 0))
                  (symbol->string (gvector-ref n 0))
                  (gvector-ref n 0))) 
            (gvector->list graph)) 0))

;Makes a new vertex with name #{vertex-name} and adds it to the graph with no incoming or outgoing edges.       
(define (make-vertex graph vertex-name)
  (if (equal? (graph-include? graph vertex-name) #f)
      (gvector-add! graph (gvector vertex-name (make-gvector) (make-gvector)))
      (begin
        (display "Error:  Vertex names must be unique.  No vertex was added to the graph.")
        (newline))))

;Adds an edge in the graph from the source vertex to the destination vertex.  If the vertices do not exist, create them
(define (make-edge graph source destination)
  (define new-source source)
  (define new-destination destination)
  (cond
    [(symbol? source) (set! new-source (symbol->string source))])
  (cond 
    [(symbol? destination) (set! new-destination (symbol->string destination))])
  (cond
    [(equal? new-source new-destination) (display "Error:  Vertices are not allowed to have self loops.  That edges (and its vertices) were not added to the graph.")]
    [(and (equal? (graph-include? graph source) #f) (equal? (graph-include? graph destination) #f))
     (begin
       (make-vertex graph source)
       (make-vertex graph destination))]
    [(equal? (graph-include? graph source) #f) (make-vertex graph source)]
    [(equal? (graph-include? graph destination) #f) (make-vertex graph destination)])
  (cond 
    [(equal? new-source new-destination) (newline)]
    [(not (equal? (index-of new-destination (map (lambda (n)
                                                     (if (symbol? n)
                                                     (symbol->string n)
                                                     n)) (get-outgoing-edges graph source)) 0) -1))
     (begin
       (display "Error:  Duplicate edges in an unweighted graph are redundant.  Redundant edge not added to the graph.")
       (newline))]
    
    ;Vertices are made, add edges appropriately 
    [else
     (begin
  (gvector-add! (gvector-ref (gvector-ref graph (get-vertex graph source)) 1) destination)
  (gvector-add! (gvector-ref (gvector-ref graph (get-vertex graph destination)) 2) source))]))

;Removes an edge in the graph from the source vertex to the destination vertex
(define (remove-edge graph source destination)
  (define new-source source)
  (define new-destination destination)
  (cond
    [(symbol? source) (set! new-source (symbol->string source))])
  (cond [(symbol? destination) (set! new-destination (symbol->string destination))])
  (define (vertex-list objects)
    (map (lambda (n)
           (if (symbol? n)
               (symbol->string n)
               n)) (gvector->list objects)))
  (cond
    [(or (equal? (graph-include? graph source) #f) (equal? (graph-include? graph destination) #f))
     (begin
       (display "Error:  No such edge exists in the graph.  No edge was removed.")
       (newline))]
    [(not (equal? (index-of new-destination (vertex-list (gvector-ref (gvector-ref graph (get-vertex graph source)) 1)) 0) -1))
      (begin
        (gvector-remove! (gvector-ref (gvector-ref graph (get-vertex graph source     )) 1) (index-of new-destination (vertex-list (gvector-ref (gvector-ref graph (get-vertex graph source)) 1)) 0))
        (gvector-remove! (gvector-ref (gvector-ref graph (get-vertex graph destination)) 2) (index-of new-source (vertex-list (gvector-ref (gvector-ref graph (get-vertex graph destination)) 2)) 0)))]))

;Removes a vertex from the graph, along with any edges going to or from that vertex
(define (remove-vertex graph vertex-name)
  (cond
    [(equal? (get-vertex graph vertex-name) -1)
     (begin
       (display "Error:  No such vertex exists within the graph.  No edge was removed.")
       (newline))]
    [else 
     (begin
       (define (iter i)
         (cond 
           [(not (>= i (gvector-count graph)))
            (begin
              (remove-edge graph (gvector-ref (gvector-ref graph i) 0) vertex-name)
              (remove-edge graph vertex-name (gvector-ref (gvector-ref graph i) 0))
              (iter (+ i 1)))]))
       (iter 0)
       (gvector-remove! graph (get-vertex graph vertex-name)))]))

;;;
;End of Graph Primitives
;;;

;;;
;Topological Sort Helper Procedures
;;;

;Creates a list [0, number] to be used to iterate over vertices in the graph with for/gvector
(define (gen-list-length number [result '()])
  (if (equal? number (length result))
      result
      (gen-list-length number (append result (list (length result))))))

;Given a gvector of strings, returns a copy of the gvector with all strings excluding name.  Used in "deleting" edges in the local-copies of graph, without modifying
;the graphs in outer environments
(define (edge-filtering vertices name)
  
  ;Removes elements of the gvector that need to be deleted
  (define (gvector-clean vertices [i 0])
    (if (>= i (gvector-count vertices))
        vertices
        (if (equal? (gvector-ref vertices i) 'NULL)
            (begin
              (gvector-remove! vertices i)
              (gvector-clean vertices i))
            (gvector-clean vertices (+ i 1)))))
  
  (let (( removed-vertices (for/gvector ((i vertices)) (if (not (equal? i name)) i 'NULL))))
    (gvector-clean removed-vertices)))

;Creates a graph where all vertex names (including the names specifying edges) are strings, while still allowing the graph in the global environment to have a mix of strings and symbols
(define (preprocess-graph graph)
  
  ;Returns name of a vertex as a string
  (define (str-map-title name)
    (if (string? name)
        name
        (symbol->string name)))
  
  ;Returns a gvector of edges, where each edge is represented as strings
  (define (str-map-gvector gvec)
    (for/gvector ([i gvec]) (if (string? i) i (symbol->string i))))
  
  ;Helper Procedure that iterates over the graph, and converts all vertices and edges to strings
  (define (iter i)
    (if (>= i (gvector-count graph))
        graph
        (begin
          (gvector-set! (gvector-ref graph i) 0 (str-map-title (gvector-ref (gvector-ref graph i) 0)))
          (gvector-set! (gvector-ref graph i) 1 (str-map-gvector (gvector-ref (gvector-ref graph i) 1)))
          (gvector-set! (gvector-ref graph i) 2 (str-map-gvector (gvector-ref (gvector-ref graph i) 2)))
          (iter (+ i 1)))))
  (iter 0))

;;;
;End Topological Sort Helper Procedures
;;;

;;;
;Sorting Procedures
;;;

;;;
;Topological Sort (Regular)
;;;

(define (topological-sort graph [visited '()])
  (let ((new-graph (make-gvector)))
    (let ((all-vertices (map (lambda (n) (gvector-ref n 0)) (gvector->list graph))))
      (map (lambda (n)
             (begin
               (gvector-add! new-graph (gvector (gvector-ref (gvector-ref graph (get-vertex graph n)) 0) (gvector-ref (gvector-ref graph (get-vertex graph n)) 1) (gvector-ref (gvector-ref graph (get-vertex graph n)) 2))) 
               n)) all-vertices)
      (let ((newer-graph (preprocess-graph new-graph)))
        (map (lambda (n)
               (begin
                 (if (not (equal? (index-of n visited) -1))
                     (begin
                       (gvector-remove! newer-graph (get-vertex newer-graph n))
                       (for/gvector ([i (gen-list-length (gvector-count newer-graph))]) 
                                    (begin
                                      (gvector-set! (gvector-ref newer-graph i) 1 (edge-filtering (gvector-ref (gvector-ref newer-graph i) 1) n))
                                      (gvector-set! (gvector-ref newer-graph i) 2 (edge-filtering (gvector-ref (gvector-ref newer-graph i) 2) n)))))
                     n))) all-vertices)
                 
        (let ((vertices (map (lambda (n) (gvector-ref n 0)) (gvector->list newer-graph))))
          (let ((sources (filter (lambda (vertex)
                           (equal? (length (get-incoming-edges newer-graph vertex)) 0)) vertices)))
            (if (equal? (length vertices) 0)
                (begin
                  (display visited)
                  (newline)
                  visited)
                (if (equal? (length sources) 0)
                  (begin
                    (display "Error:  The provided graph contains a cycle, and cannot be topologically sorted.")
                    (newline)
                    false)
                  (begin
                    (let ((new-visited visited))
                      (topological-sort graph (append new-visited (list (car sources))))))))))))))
;;;
;All Topological Sorts
;;;
(define (all-topological-sorts graph [visited '()])
(define (helper graph [visited '()])
    (let ((new-graph (make-gvector)))
      (let ((all-vertices (map (lambda (n) (gvector-ref n 0)) (gvector->list graph))))
        (map (lambda (n)
               (begin
                 (gvector-add! new-graph (gvector (gvector-ref (gvector-ref graph (get-vertex graph n)) 0) (gvector-ref (gvector-ref graph (get-vertex graph n)) 1) (gvector-ref (gvector-ref graph (get-vertex graph n)) 2))) 
                 n)) all-vertices)
        (let ((newer-graph (preprocess-graph new-graph)))
          (map (lambda (n)
                 (begin
                   (if (not (equal? (index-of n visited) -1))
                       (begin
                         (gvector-remove! newer-graph (get-vertex newer-graph n))
                         (for/gvector ([i (gen-list-length (gvector-count newer-graph))]) 
                                    (begin
                                      (gvector-set! (gvector-ref newer-graph i) 1 (edge-filtering (gvector-ref (gvector-ref newer-graph i) 1) n))
                                      (gvector-set! (gvector-ref newer-graph i) 2 (edge-filtering (gvector-ref (gvector-ref newer-graph i) 2) n)))))
                       n))) all-vertices)
          
        (let ((vertices (map (lambda (n) (gvector-ref n 0)) (gvector->list newer-graph))))
          (let ((sources (filter (lambda (vertex)
                           (equal? (length (get-incoming-edges newer-graph vertex)) 0)) vertices)))
            (if (equal? (length vertices) 0)
                (begin
                  (display visited)
                  (newline)
                  visited)
                (if (equal? (length sources) 0)
                    (begin
                      (display "Error:  The provided graph contains a cycle, and cannot be topologically sorted.")
                      (newline)
                      false)
                    (begin
                      (let ((new-visited visited))
                        (map (lambda (n)
                               (helper graph (append new-visited (list n)))) sources)))))))))))
  
(let ((result (helper graph visited)))
      (define (level-one-flatten tree)
      (cond ((empty? tree) '())
            ((not (list? (car tree))) (list tree))
            (else (append (level-one-flatten (first tree)) (level-one-flatten (rest tree))))))
  (level-one-flatten result)))

  
  ;;;
  ;End Sorting Procedures
  ;;;
  
;;;
;Test Procedures used in the debugging process
;;;

(define (make-test)
  (define test-graph (make-graph))
  (make-test-graph test-graph)
  (topological-sort test-graph)
  (all-topological-sorts test-graph))

(define (make-easy-test)
  (define easy-graph (make-graph))
  (make-easy-graph easy-graph)
  (show-graph easy-graph)
  (topological-sort easy-graph)
  (all-topological-sorts easy-graph))
   
(define (make-easier-test)
  (define easier-graph (make-graph))
  (make-easier-graph easier-graph)
  (topological-sort easier-graph)
  (all-topological-sorts easier-graph))

(define (make-cycle-test)  
  (define graph (make-graph))
  (make-edge graph "Burlington" "Lowell")
  (make-edge graph "Lowell" "Hudson")
  (make-edge graph "Hudson" "Burlington")
  (topological-sort graph)
  (all-topological-sorts graph))

(define (multi-source-test)
  (define multi-source-graph (make-graph))
  (make-edge multi-source-graph "Burlington" "Waltham")
  (make-edge multi-source-graph "Hudson" "Marlborough")
  (make-edge multi-source-graph "Burlington" "Wilmington")
  (make-edge multi-source-graph "Hudson" "Worcester")
  (topological-sort multi-source-graph)
  (all-topological-sorts multi-source-graph))

(define (make-test-graph test-graph)
  (make-edge test-graph "Calculus I" "Calculus II")
  (make-edge test-graph "Computing II" "Computing III")
  (make-edge test-graph "Computing II" "Assembly Language Programming")
  (make-edge test-graph  "Calculus II" "Logic Design")
  (make-edge test-graph "Assembly Language Programming" "Computer Architecture")
  (make-edge test-graph "Logic Design" "Computer Architecture")
  (make-edge test-graph "Computing III" "Computing IV")
  (make-edge test-graph "Calculus I" "Discrete I")
  (make-edge test-graph "Prob and Stats" "Analysis of Algorithms")
  (make-edge test-graph "Discrete I" "Discrete II")
  (make-edge test-graph "Calculus II" "Prob and Stats")
  (make-edge test-graph "Computer Architecture" "Operating Systems")
  (make-edge test-graph "Computing IV" "OPL")
  (make-edge test-graph "Computing II" "Analysis of Algorithms")
  (make-edge test-graph "Discrete II" "Foundations")
  (make-edge test-graph "Computing I" "Computing II"))

(define (make-easy-graph easy-graph)
  (make-edge easy-graph "Hudson" "Chelsea's House")
  (make-edge easy-graph "Lowell" "UMass")
  (make-edge easy-graph "Burlington" "Hudson")
  (make-edge easy-graph "Burlington" "Lowell"))  

(define (make-easier-graph easier-graph)
  (make-edge easier-graph "Burlington" "Lowell")
  (make-edge easier-graph "Burlington" "Hudson")
  (make-edge easier-graph "Lowell" "Umass")
  (make-edge easier-graph "Hudson" "Umass"))

;;;
;End Test Procedures
;;;