#lang racket

(require pict)
(require pict/tree-layout)
;(require "test-srts.rkt")
(require "util.rkt")
(require "course.rkt")
(require "al-doerr.rkt")
(define TXTSIZE 15)
(define TXT 15)

(define (vis-prereq str len)
  (let* [(circ (colorize (disk (* len TXTSIZE)) "orange"))
         (txt (text str null TXT 0))]
    (cc-superimpose circ txt)))

(define (vis-level-from-top lst len)
  (define (vis-tree req)
    (cons (vis-prereq (string-append
                       (number->string (list-index req lst))
                       ": "
                       (course-number req)
                       )
                      len)
          (list (if (null? (course->prereqs-list req)) '()
              (map vis-tree (course->prereq-courses req))))))
  (define (combine req)
        (apply tree-layout #:pict (car req)
                     (if (null? (cadr req))
                         (list #f)
                         (map combine (cadr req)))))
  (combine (vis-tree (last lst))))
  
;(define (vis-most-coherent sorts)
;  (let* [(srt (argmin coherence sorts))
;         (maxlen (apply max (map (lambda (lst)
;                            (string-length
;                             (car lst))) srt)))
;         (top-req (last srt))]
;    (vis-level-from-top top-req maxlen)))
    
;(define reqs (vis-most-coherent sorts))
;(define vis-tree (naive-layered reqs #:y-spacing 50))
;(show-pict vis-tree)
(provide (all-defined-out))