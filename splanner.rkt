;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  CourseGrid:splaner.rkt                                          ;;
;;                                                                  ;;
;;  Purpose: Main application.                                      ;;
;;                                                                  ;;
;;  Author: Willie Boag                           wboag@cs.uml.edu  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#lang racket


(require "course.rkt")
(require "scraper.rkt")
(require "al-doerr.rkt")
(require "topologicalsort.rkt")
(require "util.rkt")


(define courses (get-courses))


; ANNOTATED
; mathematics
; electrical-engineering-and-computer-science


#|
; list of undergraduate courses (as course objects)
;(define dept "electrical-engineering-and-computer-science")
(define dept "linguistics-and-philosophy")
(define all-courses (department->courses dept))
;(displayln all-courses)

(cond ((not (equal? all-courses 'BAD-QUERY))
       (set! courses (filter (lambda (c) 
                                 (equal? "Undergraduate" (course-level c)))
                               all-courses))

       ;(set! courses (filter (lambda (c)
       ;                        (equal? "6.837" (course-number c)))
       ;                        courses))
       
       ; list of all of the official course numbers
       ;(define course-numbers (map course-number        courses))
       
       ; list of prerequisite descrptions
       ;(define prereq-texts   (map course->prereqs-text courses))
       
       ; list of (list of prerequisites for each course)
       ;(define prereq-lists   (map course->prereqs-list courses))
       
       #|
       ; display data
       (for-each (lambda (c t) 
                   (displayln (course-name   c)) 
                   (displayln (course-number c))
                   (displayln (course-url    c))
                   (displayln t)
                   (displayln "----------")) 
                 courses prereq-texts)
       |#
       
       
       #|
       (for-each (lambda (c t p) 
                   (if (equal? p p)
                       (begin
                         (displayln (course-name   c)) 
                         (displayln (course-number c))
                         (displayln (course-url    c))
                         (displayln t)
                         (displayln p)
                         (displayln "----------")
                         ) 
                       void))
                 courses prereq-texts prereq-lists)
       |#
       
       
       void))
|#





(set! courses (department->courses "electrical-engineering-and-computer-science"))



; accumulate prereq closure
(define (all-prereqs course)
  (let ((prereqs (course->prereqs-list course)))
    (if (null? prereqs)
        '()
        ; add current prereqs to return value and get transitive closure
        ; NOTE: hack of taking first course from OR relationship
        (append
         (map (lambda (c) (list (car c) (course-number course)))
              prereqs)
         (foldl append '() (map (lambda (c-num)
                                  (all-prereqs 
                                   (get-course-from-number 
                                    (car c-num))))
                                prereqs))))))




; REPL
; TODO: Add lots of different commands 
;       ex. (get class from number, lookup number from keywords, etc)
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
                (best-sort-nums (map course-number best-sort-courses)))
           (displayln best-sort-nums)
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
