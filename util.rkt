#lang racket

(require "test-srts.rkt")
(require "al-doerr.rkt")

;(define (list-index len ele lst)
;  (- len (length (memq ele lst))))

(define (list-index ele lst)
  (- (length lst) (length (memq ele lst))))

(define (coherence srt)
  (foldr + 0
         (map
          (lambda (req)
            (let [(index (list-index req srt))
                  (reqlen (length (course->prereq-courses req)))]
              (foldl + 0
                     (map
                      (lambda (ele) (- index (list-index ele srt)))
                      (course->prereq-courses req)))))
          srt)))

;(map coherence sorts)

(provide (all-defined-out))
