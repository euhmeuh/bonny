#lang racket/base

(provide
  logs-page)

(require
  bonny/pirate
  "_base.rkt")

(define (logs-page [pirate #f])
  (base-page "Logs" '()
    (lambda ()
      `(main
         ,(if pirate
            `(h2 "Logs for pirate " ,(pirate-name pirate))
            '(h2 "Bonny's own logs"))
       ))))
