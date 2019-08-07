#lang racket/base

(provide
  trails-page)

(require
  bonny/pirate
  "_base.rkt")

(define (trails-page pirate)
  (base-page "Trails" '()
    (lambda ()
      `(main
         (h2 "Trails for pirate " ,(pirate-name pirate))
       ))))
