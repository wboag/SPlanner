;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  CourseGrid:url-lib.rkt                                                ;;
;;                                                                        ;;
;;  Purpose: Wrapper for url library that caches results.                 ;;
;;           Cached results prevent spamming site during development.     ;;
;;                                                                        ;;
;;  Author: Willie Boag                                 wboag@cs.uml.edu  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require net/url)

(provide url->port)
(provide read-url)


; Remove a set of characters from a target string
(define (remove-chars str to-remove)
  (let
      ((dont-include (string->list to-remove)))
    (list->string (map (lambda (c) (if (memq c dont-include)
                                       #\_
                                       c))
                          (string->list str)))))


; convert a url string to a filename to save to disk
(define (url->cache-entry s)
  (string-append "url_cache/"
                 (string-trim (remove-chars s "/\\~:")
                              "_")))
        

; get the HTML data from a given URL
(define (url->port s)
  (let
      ((filename (url->cache-entry s)))
    
    ; does file already exist in cache?
    (cond 
      ((not (file-exists? (string->path filename)))
       (let
           ((html-data (port->bytes (get-pure-port (string->url s) 
                                                   #:redirections 5))))
         ; Save data to cache
         (display-to-file html-data filename))))
    
    ; get from local cache
    (open-input-file filename)))


(define (read-url s)
  (port->string (url->port s)))