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
(require "annotations.rkt")



; test whether two strings represent courses from the same department
; ex. (same-department? "18.01" "18.63A")  -->   true)
; ex. (same-department? "18.01" "6.046")   -->  false)
(define (same-department? num1 num2)
  (let
      ((d1 (regexp-match #rx"[0-9]+\\." num1))
       (d2 (regexp-match #rx"[0-9]+\\." num2)))
    (equal? d1 d2)))



; convert course id string into two-decimal numeric value
; ex. "18.013A" -> "18.01"
(define (numeric-value course-id)
  (let
      ((num   (regexp-match #rx"[0-9]+\\.[0-9][0-9]+" course-id))
       (goofy (regexp-match #rx"[0-9]+\\.S[0-9][0-9]" course-id)))
    (cond
      ; Heuristic: Assume xx.Sxx is largest course number in dept.
      (goofy (+ (string->number (car (regexp-match #rx"[0-9]|" course-id)))
                .999))
      (num   (string->number (car num)))
      (else  (error "Could not extract course number")))))



; Get list of prereqs (strings) from a course object
(define (course->prereqs-list course)
  ; already done?
  (if (hand-annotated? course)
      ; if available: lookup of hand-annotated preqs
      (get-annotations course)

      ; else: attempt automatic prereq extraction
      (let
          ((course-id (course-number course))
           (descr (course->prereqs-text course)))
        (displayln descr)
        (if (eq? descr 'COULD-NOT-FIND-SYLLABUS)
            'UNKNOWN
            (let
                ((course-no (numeric-value course-id))
                 (numbers (regexp-match* #rx"[0-9]+\\.[0-9]+[A-Z]*" descr)))
              (let
                  ((filtered (filter (lambda (n) 
                                       ; Heuristic: Only consider inter-department
                                       (and (same-department? course-id n)
                                            (< (numeric-value n) course-no)))
                                     numbers)))
                ; OR relationship?
                (if (regexp-match "(?i: or )" descr)
                    (if (not (empty? filtered))
                        (list filtered) ; if OR occurs in text, apply OR to all
                        '())
                    (map list filtered)
                    )))))))
  

; get a course object with a given course number
(define (get-course-from-number num)
  ; ASSUMPTION: Global course list is defined already
  (let ((matches (filter (lambda (c) (equal? (course-number c)
                                            num))
                        courses)))
    (if (not (empty? matches))
        (car matches)
        false)))

; ANNOTATED
; mathematics


; list of undergraduate courses (as course objects)
(define dept "electrical-engineering-and-computer-science")
;(define dept "mathematics")
(define all-courses (department->courses dept))
;(displayln all-courses)

(define courses '())
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
       (define prereq-texts   (map course->prereqs-text courses))
       
       ; list of (list of prerequisites for each course)
       (define prereq-lists   (map course->prereqs-list courses))
       
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
       
       
       (for-each (lambda (c t p) 
                   (if (equal? p 'UNKNOWN)
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
       
       
       void))





#|
; REPL
; TODO: Add lots of different commands 
;       ex. (get class from number, lookup number from keywords, etc)
(define (repl)
  (displayln "What course would you like to know about? ")
  (let
      ((input (read-line)))
    (if (equal? input eof)
        true
        (let ((course (get-course-from-number input)))
          (if course
              (begin
                (displayln "You chose")
                (displayln (course-name course))
                (display "prerequisites: ")
                (displayln (course->prereqs-list course)))
              (begin
                (displayln (string-append "I'm sorry. "
                                          "I don't recognize "
                                          "course " 
                                          input))))
          (repl)))))
(repl)
(displayln "Happy learning!")
|#