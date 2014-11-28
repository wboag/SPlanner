;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  CourseGrid:html-lib.rkt                                               ;;
;;                                                                        ;;
;;  Purpose: Parse html data.                                             ;;
;;                                                                        ;;
;;  Author: Willie Boag                                 wboag@cs.uml.edu  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#lang racket


(require (planet neil/html-parsing:2:0))


(provide eliminate-duplicate-whitespace)
(provide shallow-get-text)
(provide    deep-get-text)
(provide get-attr)
(provide get-attr-all)
(provide get-attr-path)
(provide get-field)


; Parse HTML (because I couldn't get the sxpath library to work
(define (get-attr ht-data attribute)
  (let
      ((result (get-attr-all ht-data attribute)))
    (cond ((equal? result 'BAD-QUERY) 'BAD-QUERY)
          ((and (list? result) (= (length result) 0)) 'BAD-QUERY)
          ((list? result) (car result))
          (else 'ERROR-IN-GET-ATTR))))


; Get all children with a given attribute
(define (get-attr-all ht-data attribute)
  (let
      ; attribute & optional fields
      ((attr   (if (pair? attribute) (car attribute) attribute))
       (fields (if (pair? attribute) (cdr attribute)      '() )))
    
    ; if error
    (if (equal? ht-data 'BAD-QUERY)
        ; propogate error to top
        'BAD-QUERY
    
        ; get all children with the given attributes (and any pairs, if necesarry)
        (filter (lambda (data) (and (pair? data) 
                                    (equal? (car data) attr)
                                    (has-fields? data fields)))
                ht-data))))
  
         
; (get-attr-path doc '(html body (div (@ (id "ResultsSection")))))
(define (has-fields? data fields)
  (define (iter d f)
    (if (null? f)
        #t
        (and (member (car fields) d)
             (iter d (cdr fields)))))
  (iter (filter (lambda (it) 
                  (and (pair? it) 
                       (equal? (car it) '@))) 
                data)
        fields))


; get value of field
(define (get-field data field)
  (let
      ((fields (filter (lambda (d) (and (pair? d) 
                                        (equal? (car d) '@)))
                       data)))
    (if (null? fields)
        "NO-FIELD"
        (cadar (filter (lambda (p) (equal? (car p) field))
                      (cdar fields))))))


; Query of multiple hops
(define (get-attr-path ht-data path)
  (define (iter p result)
    (cond ((null? p) result)                 ; success
          ((equal? p 'BAD-QUERY) 'BAD-QUERY) ; failure
          (else (iter (cdr p) (get-attr result (car p))))))
  (iter path ht-data))



; Get Shallow text from HTML object
(define (shallow-get-text ht-data)
  (define (iter ht result)
    (if (null? ht)
        result
        (iter (cdr ht) (if (string? (car ht)) (string-append result (car ht)) result))))
  (iter ht-data ""))
      

(define (deep-get-text ht-data)
  (cond 
    ((pair? ht-data)
     (if (equal? (car ht-data) '@)
         ""
         (string-join (map deep-get-text ht-data))))
    ((string? ht-data)
     ht-data)
    (else
     "")))




; Eliminate all adjacent whitespace (ex. "hi   man" -> "hi man"
(define (eliminate-duplicate-whitespace w)
  (string-join (string-split w)))
