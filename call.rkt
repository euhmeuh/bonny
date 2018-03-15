#lang racket/base

(provide
  call)

(require
  racket/system)

(define (call #:dir [dir #f]
              command . args)
  (parameterize ([current-directory (or dir (current-directory))])
    (system (apply format (cons command args)) #:set-pwd? #t)))
