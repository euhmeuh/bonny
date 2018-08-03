#lang racket/base

(provide
  index-page)

(require
  "_base.rkt")

(define (index-page)
  (base-page "Main desk" '()
    (lambda ()
      `(main
         (h2 "Main desk")))))
