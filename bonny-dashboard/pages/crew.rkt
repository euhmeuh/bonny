#lang racket/base

(provide
  crew-page)

(require
  bonny/pirate
  "_base.rkt")

(define (render-pirate pirate)
  (define name (pirate-name pirate))
  `(tr
     (th ([scope "row"]) ,(symbol->string name))
     (td ,(pirate-repo-url pirate))
     (td "Running")
     (td (a ([href ,(format "/pirate/~a" name)]) "Stats"))
     (td (a ([href ,(format "/pirate/~a/manage" name)]) "Manage"))
     (td (a ([href ,(format "/pirate/~a/logs" name)]) "Logs"))))

(define (crew-page pirates)
  (base-page "Ship's crew" '()
    (lambda ()
      `(main
         (h2 "Ship's crew")
         (table
           (thead
             (tr
               (th ([scope "col"]) "Pirate name")
               (th ([scope "col"]) "Repository URL")
               (th ([scope "col"]) "Status")
               (th ([scope "col"] [colspan "3"]) "Actions")))
           (tbody ,@(map render-pirate pirates)))))))
