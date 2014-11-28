;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  CourseGrid:course.rkt                                                 ;;
;;                                                                        ;;
;;  Purpose: Object for storing data about a given course.                ;;
;;                                                                        ;;
;;  Author: Willie Boag                                 wboag@cs.uml.edu  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket



(provide make-course)
(provide course-name course-number course-url course-level)
(provide display-c displayln-c)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Course Object                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; constructor
(define (make-course name number url level)
  (lambda (message)
    (cond
      ((eq? message 'name  )   name)
      ((eq? message 'number) number)
      ((eq? message 'url   )    url)     
      ((eq? message 'level )  level)
      (else
       (error "Method not available for course object")))))


; accessors
(define (course-name   c) (c 'name  ))
(define (course-number c) (c 'number))
(define (course-url    c) (c 'url   ))
(define (course-level  c) (c 'level ))


; display object
(define (display-c c)
  (displayln (list (course-name   c)
                   (course-number c)
                   (course-url    c)
                   (course-level  c))))
(define (displayln-c c)
  (display-c c)
  (newline))