;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  CourseGrid:annotations.rkt                                            ;;
;;                                                                        ;;
;;  Purpose: Interface for using hand-annotated prereq data.              ;;
;;                                                                        ;;
;;  Author: Willie Boag                                 wboag@cs.uml.edu  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#lang racket


(require "course.rkt")
(require "scraper.rkt")


(provide hand-annotated?)
(provide get-annotations)



; has the given course been manually annotated?
(define (hand-annotated? c)

  ; if course number's data not loaded, then load
  (let ((dept (number->department (course-number c))))
    (cond ((not (is-module-loaded? dept))
           (load-module dept))))
  
  ; determine if course prereqs are available
  (hash-has-key? memo (course-number c)))



; get the list of preprequisites for a given course
(define (get-annotations c)
  
  ; if course number's data not loaded, then load
  (let ((dept (number->department (course-number c))))
    (cond ((not (is-module-loaded? dept))
           (load-module dept))))
  
  ; lookup prerequisites for given course
  (hash-ref memo (course-number c) false))



; map course department number to department name
(define depts (make-hash))
(hash-set! depts "18" "mathematics")
(hash-set! depts "6"  "electrical-engineering-and-computer-science")

(define (number->department course-no)
  (let ((dept-no (regexp-match #rx"[0-9]+" course-no)))
    (if (empty? dept-no)
        'BAD-INPUT
        (hash-ref depts (car dept-no) 'UNRECOGNIZED-DEPT))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;         Lookup Table: course number -> prerqusites          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; build global annotations hash table
(define memo (make-hash))


; load prereqs from a file
(define (read-prereqs port)
  
  ; read one course from a file
  (define (read-course port)
    (define (build-prereqs course-number result)
      (let ((line (read-line port))
            (res (if (equal? result '(("UNKNOWN")))
                     'UNKNOWN
                     result)))
        (cond 
          ; if done reading course & at end of file
          ((equal? line eof)
           (hash-set! memo course-number res)
           eof)
          ; if done reading this course & still more to go
          ((equal? line "----------")
           (begin
             (hash-set! memo course-number res)
             true))
          ; if more to read
          (else
           (build-prereqs course-number 
                          (cons (string-split line " ")
                                result))))))
    
    ; read course header (only the course number actually matters)
    (let ((name   (read-line port))
          (number (read-line port))
          (text   (read-line port)))    
      ; returns eof when done with whole file
      (build-prereqs number '())))
  
  ; each iteration adds another (course-no,prereq) pair to 'memo'
  (define (iter)
    (if (equal? (read-course port) eof)
        void
        (iter)))
  (iter))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;           Module caches (only load when asked for)          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; list of loaded modules
(define loaded-modules '())

; determine whether module has been loaded
(define (is-module-loaded? mod-name)
  (if (memq mod-name loaded-modules) true false))

; load given module
(define (load-module mod-name)
  (cond ((not (is-module-loaded? mod-name))
         (begin
           ; load module
           (read-prereqs 
            (open-input-file 
             (string-append "data/annotations/" 
                            mod-name
                            ".txt")))
           ; mark module as loaded
           (set! loaded-modules (cons mod-name loaded-modules))
           )))
  true)

;(read-prereqs (open-input-file "data/annotations/mathematics.txt"))
