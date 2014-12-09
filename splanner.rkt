;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  CourseGrid:splaner.rkt                                          ;;
;;                                                                  ;;
;;  Purpose: Main application.                                      ;;
;;                                                                  ;;
;;  Author: Willie Boag                           wboag@cs.uml.edu  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#lang racket

(require pict)
(require pict/tree-layout)
(require "course.rkt")
(require "scraper.rkt")
(require "al-doerr.rkt")
(require "topologicalsort.rkt")
(require "util.rkt")
(require "vis.rkt")

(define courses (get-courses))



(set! courses (department->courses "electrical-engineering-and-computer-science"))



; driver
(define (repl)
  (displayln "What action would you like to select? ")
  (let
      ((input (read-line)))
    (cond 

      ; done
      ((or (equal? input eof) (equal? input "exit"))
       true)
      
      ; help
      ((equal? input "help")
       (begin 
         (displayln "Actions:")
         (displayln " lookup   - find course with given course number (ex. 18.02)")
         (displayln " keywords - find courses about particular keyword")
         (displayln " load     - load data for a given department")
         (displayln " sequence - get SPlanner's recommendations for course prequisit order")
         (displayln " exit     - close SPlanner")
         (newline)
         (repl)))

      ; load department data
      ((equal? input "load")
       (begin
         (displayln "From which department would you like to load data?")
         (let ((dept (read-line)))
           ; FIXME - dont reload courses
           (set! courses (append courses (department->courses dept)))
           (newline)
           (repl))))
 
      ; recommended sequence
      ((equal? input "sequence")
       (begin
         (displayln "Please enter the target course number.")
         (let* ((course-no (read-line))
                (target (get-course-from-number course-no))
                (dependency-list (set->list (list->set (all-prereqs target))))
                (course-graph (make-graph-with-edges dependency-list))
                (all-sorts (all-topological-sorts course-graph))
                (all-sorts-courses (map (lambda (s)
                                          (map get-course-from-number s))
                                        all-sorts))
                (best-sort-courses (argmin coherence all-sorts-courses))
                (best-sort-nums (map course-number best-sort-courses))
                (maxlen (apply max (map string-length best-sort-nums)))
                (vis-courses (vis-level-from-top best-sort-courses maxlen))
                (vis-tree (linewidth 3 (naive-layered vis-courses #:y-spacing 5))))
           (print vis-tree)
           (newline)
           (repl))))
      
      ; keyword search
      ((equal? input "keywords")
       (begin
         (displayln "Please enter a set of keywords to search")
         (let* ((keys (string-split (string-downcase (read-line))))
                (relevant (filter (lambda (c)
                                    (foldl (lambda (a b) (or a b))
                                           #f
                                           (map (lambda (word)
                                                  (member word keys))
                                                (string-split 
                                                 (string-downcase
                                                  (course-name
                                                   c))))))
                                  courses)))
           (map (lambda (c) 
                  (displayln (list (course-name c) (course-number c)))) 
                relevant)
           (newline)
           (repl))))

      ; lookup from course number
      ((equal? input "lookup")
       (begin
         (displayln "What course would you like to know about? ")
         (let* ((course-no (read-line))
                (course (get-course-from-number course-no)))
           (cond 
             ((symbol? course)
              (begin
                (displayln (string-append "I'm sorry. "
                                          "I don't recognize "
                                          "that department "))))
             ((equal? course false)
              (begin
                (displayln (string-append "I'm sorry. "
                                          "I don't recognize "
                                          "course " 
                                          input))))
             (else
              (begin
                (displayln "You chose")
                (displayln (course-name course))
                (display "prerequisites: ")
                (displayln (course->prereqs-list course)))))
           (newline)
           (repl))))
      
      (else
       (displayln (string-append "unrecognized command "
                                 input))
       (newline)
       (repl)))
    ))
(define ans (repl))
(displayln "Happy learning!")
