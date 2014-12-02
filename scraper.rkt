;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  CourseGrid:scraper.rkt                                                ;;
;;                                                                        ;;
;;  Purpose: Crawl MIT OCW to gather data about available courses.        ;;
;;                                                                        ;;
;;  Author: Willie Boag                                 wboag@cs.uml.edu  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#lang racket


(require (planet neil/html-parsing:2:0))

(require "url-lib.rkt")
(require "html-lib.rkt")
(require "course.rkt")


(provide department->courses)
(provide course->prereqs-text)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              Scrape department page for course listings                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

         
; Extract course name, number, level, and URL from html block
; ex (html->course <string-of-html>)   ->   <course-obj>
(define (html->course ht-data)
  (let
      ((course-name  (eliminate-duplicate-whitespace
                       (shallow-get-text 
                        (get-attr (cadr (get-attr-all ht-data 'td))
                                  'a))))
       (course-number (eliminate-duplicate-whitespace 
                       (shallow-get-text (get-attr-path ht-data '(td a)))))
       (course-url (resolve-ocw-url 
                    (get-field (get-attr (caddr (get-attr-all ht-data 'td)) 'a) 
                               'href)))
       (undergrad? (eliminate-duplicate-whitespace 
                    (shallow-get-text (get-attr (caddr (get-attr-all ht-data 'td)) 
                                                'a)))))
    (make-course course-name course-number course-url undergrad?)))




; convert relative URL from website to absolute URL
(define (resolve-ocw-url url)
  (string-append "http://ocw.mit.edu" url "/"))



; department name string to list of course objects
; ex. (department->courses "mathematics")  -->  '(<course-A> <course-B> ... <course-N>)
(define (department->courses department)
  (let ((html-nested-toks (html->xexp     ; read url
                           (url->port
                            (string-append "http://ocw.mit.edu/courses/"
                                department)))))
    ;(displayln (string-append "http://ocw.mit.edu/courses/"
    ;                            department))
    ;(displayln html-nested-toks)
    (let ((html-for-courses 
           (get-attr-path html-nested-toks
                          '(html body (div (@ (id "center_global"))) 
                                 (div (@ (id "grid"))) 
                                 (div (@ (id "left"))) 
                                 (div (@ (class "global_wrapper"))) 
                                 (div (@ (id "global_inner"))) 
                                 (table (@ (class "courseList"))) 
                                 tbody ))))

      ; error?
      (if (equal? html-for-courses 'BAD-QUERY)
          ; propogate error to top
          'BAD-QUERY
      
          ; get data from each course
          (map html->course
               
               ; go to section of html containing course data
               ; argument to get-attr-all is "BAD-QUERY"
               (get-attr-all html-for-courses 'tr))))))
    



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                Scrape course home page to find syllabus                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Get the syllabus for a given course
; (get-syllabus <course-obj>)   -->   <string-url-to-syllabus>
(define (get-syllabus c)
  (let*
      ((url (course-url c))
       (parsed (html->xexp (url->port url))) 
       (path (get-attr-path parsed '(html body (div (@ (id "center_chp"    ))) 
                                               (div (@ (id "grid"          )))
                                               (div (@ (id "left"          )))
                                               (div (@ (id "course_wrapper"))) 
                                               (div (@ (id "course_nav"    )))
                                               ul
                                               (li  (@ (class ""           )))
                                               a))))
    (if (equal? path 'BAD-QUERY)
        'BAD-QUERY
        (resolve-ocw-url (get-field path 'href)))))


; Get the written prereq text for a given course
;  ex. (course->prereqs-text <course-obj>)   -->   <string-of-prereq-text>
(define (course->prereqs-text c)
  (let*
      ((url (get-syllabus c)))
    (if (equal? url 'BAD-QUERY)
        'COULD-NOT-FIND-SYLLABUS
        (let*
            ((parsed (html->xexp (url->port url))) 
             (path (get-attr-path parsed '(html body (div (@ (id "center"           ))) 
                                                (div (@ (id "grid"                  )))
                                                (div (@ (id "left-section"          )))
                                                (div (@ (id "course_wrapper_section"))) 
                                                (div (@ (id "course_inner_section"  ))) 
                                                ))))
          
          (define (printer t)
            (cond
              ((pair? t) (eliminate-duplicate-whitespace (deep-get-text t)))
              ((string? t) (eliminate-duplicate-whitespace t))
              (else t)))
          
          (define (is-non-whitespace? s)
            (if (string? s)
                (not (equal? "" (eliminate-duplicate-whitespace s)))
                #f))
          (let 
              ((strings (filter is-non-whitespace? (map printer path))))
            ;(displayln strings)
            ; NOTE: cadr assumption assumes all in one block
            (cond 
              ((member "Prerequisites" strings)
               (cadr (member "Prerequisites" strings)))
              ((member "Prerequisite" strings)
               (cadr (member "Prerequisite" strings)))
              (else
               'COULD-NOT-FIND-PREREQUISITES)
              ))))))

