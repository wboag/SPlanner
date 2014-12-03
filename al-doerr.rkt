#lang racket


(require "annotations.rkt")
(require "course.rkt")
(require "scraper.rkt")

(provide get-courses)
(provide course->prereqs-list)
(provide get-course-from-number)
(provide course->prereq-courses)


; List of loaded courses
(define courses '())

(define (get-courses)
  courses)


; test whether two strings represent courses from the same department
; ex. (same-department? "18.01" "18.63A")  -->   true)
; ex. (same-department? "18.01" "6.046")   -->  false)
(define (same-department? num1 num2)
  (let
      ((d1 (regexp-match #rx"[\\.]+\\." (string-upcase num1)))
       (d2 (regexp-match #rx"[\\.]+\\." (string-upcase num2))))
    (equal? d1 d2)))



; convert course id string into two-decimal numeric value
; ex. "18.013A" -> "18.01"
(define (numeric-value course-id)
  (let
      ((num   (regexp-match #rx"[^\\.]+\\.[0-9][0-9]+" 
                            (string-upcase course-id)))
       (goofy (regexp-match #rx"[^\\.]+\\.S[0-9][0-9]" 
                            (string-upcase course-id))))
    (cond
      ; Heuristic: Assume xx.Sxx is largest course number in dept.
      (goofy (+ (string->number (car (regexp-match #rx"[0-9]|" course-id)))
                .999))
      (num   (string->number (car num)))
      (else  (error "Could not extract course number")))))



; Get list of prereqs (strings) from a course object
; NOTE: In this file, because it uses hand-annotated?
;       hand-annotated? is defined in annotations.rkt, which requires
;       scraper.rkt. Therefore, this cannot go in scraper.rkt without
;       causing a circular dependency.
(define (course->prereqs-list course)
  ; already done?
  (if (hand-annotated? course)
      ; if available: lookup of hand-annotated preqs
      (get-annotations course)

      ; else: attempt automatic prereq extraction
      (let
          ((course-id (course-number course))
           (descr (course->prereqs-text course)))
        (if (symbol? descr)
            ; error getting prereqs?
            '()
            ; prereqs text found; extract data
            (let
                ((course-no (numeric-value course-id))
                 (numbers (regexp-match* #rx"[\\.]+\\.[0-9]+[A-Z]*" descr)))
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
  ; First try: determine if course data has already been fetched
  (let ((matches (filter (lambda (c) (equal? (course-number c)
                                             (string-upcase num)))
                        courses)))
    ; if course data already fetched?
    (if (not (empty? matches))
        (car matches)
        ; else: retrieve courses for given department and re-check
        (begin
          ; retrieve all deprtment courses
          (let ((dept (number->department num)))
            (if (symbol? dept)
                dept
                (let ((dept-courses (department->courses dept)))
                  (set! courses (append dept-courses courses))
                  ; Second try: determine if course data is available
                  (let 
                      ((second-matches (filter (lambda (c)
                                                 (equal? (course-number c)
                                                         (string-upcase num)))
                                               courses)))
                    ; if course data already fetched?
                    (if (not (empty? second-matches))
                        (car second-matches)
                        false)))))))))
    


; map course department number to department name
(define depts (make-hash))
(hash-set! depts "4"  "architecture")
(hash-set! depts "MAS" "media-arts-and-sciences") ; FIXME: generalize
(hash-set! depts "11" "urban-studies-and-planning")
(hash-set! depts "16" "aeronautics-and-astronautics")
(hash-set! depts "20" "biological-engineering")
(hash-set! depts "10" "chemical-engineering")
(hash-set! depts "1" "civil-and-environmental-engineering")
(hash-set! depts "6"  "electrical-engineering-and-computer-science")
(hash-set! depts "ESD" "engineering-systems-division") ; FIXME: generalize
(hash-set! depts "HST" "health-sciences-and-technology") ; FIXME: generalie
(hash-set! depts "3" "materials-science-and-engineering")
(hash-set! depts "2" "mechanical-engineering")
(hash-set! depts "22" "nuclear-engineering")
(hash-set! depts "21A" "anthropology") ; FIXME: generalize
(hash-set! depts "CMS" "comparative-media-studies") ; FIXME: generalize
(hash-set! depts "14" "economics")
(hash-set! depts "21F" "foreign-languages-and-literatures") ; FIXME: generalize
(hash-set! depts "21H" "history") ; FIXME: generalize
(hash-set! depts "24" "linguistics-and-philosophy")
(hash-set! depts "21L" "literature") ; FIXME: generalize
(hash-set! depts "21M" "music-and-theater-arts") ; FIXME: generalize
(hash-set! depts "17" "political-science")
(hash-set! depts "STS" "science-technology-and-society") ; FIXME: generalize
(hash-set! depts "21W" "writing-and-humanistic-studies") ; FIXME: generalize
(hash-set! depts "WGS" "womens-and-gender-studies") ; FIXME: generalize
(hash-set! depts "7" "biology")
(hash-set! depts "9" "brain-and-cognitive-sciences")
(hash-set! depts "5" "chemistry")
(hash-set! depts "12" "earth-atmospheric-and-planetary-sciences")
(hash-set! depts "18" "mathematics")
(hash-set! depts "8" "physics")
(hash-set! depts "15" "sloan-school-of-management")
(hash-set! depts "PE" "athletics-physical-education-and-recreation") ; FIXME: generalize
(hash-set! depts "CC" "concourse") ; FIXME: generalize
(hash-set! depts "ES" "experimental-study-group") ; FIXME: generalize
(hash-set! depts "SP" "special-programs")




(define (number->department course-no)
  (let
      ((dept-no (regexp-match #rx"[^\\.]+" (string-upcase course-no))))
    ;(displayln dept-no)
    (if (empty? dept-no)
        'BAD-INPUT
        (hash-ref depts (string-upcase (car dept-no)) 'UNRECOGNIZED-DEPT))))



(define (course->prereq-courses c)
  (map get-course-from-number (map car (course->prereqs-list c))))