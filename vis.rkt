#lang racket

(require pict)
(require pict/tree-layout)
(require "test-srts.rkt")
(require "util.rkt")

(define TXTSIZE 10)
(define TXT 15)

(define (vis-prereq sym len)
  (let* [(str (symbol->string sym))
         (circ (colorize (disk (* len TXTSIZE)) "orange"))
         (txt (text str null TXT 0))]
    (cc-superimpose circ txt)))

(define (gen-max lst func)
  (let [(high -inf.0)
        (top '())]
    (for-each
     (lambda (arg)
       (let [(val (func arg))]
         (cond [(> val high) (set! high val) (set! top arg)])))
     lst)
    top))

(define (vis-level-from-top req len)
  (define (vis-tree req)
    (cons (vis-prereq (car req) len)
          (list (if (null? (cadr req)) '()
              (map vis-tree (cadr req))))))
  (define (combine req)
        (apply tree-layout #:pict (car req)
                     (if (null? (cadr req))
                         (list #f)
                         (map combine (cadr req)))))
  (combine (vis-tree req)))
  

(define (vis-most-coherent sorts)
  (let* [(srt (gen-max sorts coherence))
         (maxlen (apply max (map (lambda (lst)
                            (string-length
                             (symbol->string (car lst)))) srt)))
         (top-req (last srt))]
    (vis-level-from-top top-req maxlen)))
    
(define reqs (vis-most-coherent sorts))
(define vis-tree (naive-layered reqs #:y-spacing 50))
(show-pict vis-tree)