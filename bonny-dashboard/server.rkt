#lang racket/base

(require
  web-galaxy/serve
  web-galaxy/response
  bonny/pirate
  bonny/utils)

(define pirates (map (lambda (p) (apply pirate p))
                     (read-sexp-file "pirates")))

(define-response (index)
  (local-require "pages/index.rkt")
  (response-page (index-page)))

(define-response (crew)
  (local-require "pages/crew.rkt")
  (response-page (crew-page pirates)))

(define-response (not-found)
  (local-require "pages/_base.rkt")
  (response-page
    (base-page "404 Not found" '()
      (lambda ()
        '(main (p "404 Not found - The page you requested was not found."))))))

(parameterize ([current-server-static-paths (list "static")]
               [current-not-found-responder response-not-found])
  (serve/all
    [("") response-index]
    [("crew") response-crew]))
