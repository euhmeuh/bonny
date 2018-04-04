#lang racket/base

(require
  web-server/servlet
  web-server/servlet-env
  web-server/managers/none
  web-server/configuration/responders
  "../models/pirate.rkt"
  "../utils.rkt")

(define pirates (map (lambda (p) (apply pirate p))
                     (read-sexp-file "pirates")))

(define-syntax-rule (response-page content)
  (response/xexpr
    #:preamble #"<!DOCTYPE html>"
    content))

(define (response-index req)
  (local-require "pages/index.rkt")
  (response-page (index-page)))

(define (response-crew req)
  (local-require "pages/crew.rkt")
  (response-page (crew-page pirates)))

(define (response-not-found req)
  (local-require "pages/_base.rkt")
  (response/xexpr
    (base-page "404 Not found" '()
      (lambda ()
        '(main (p "404 Not found - The page you requested was not found."))))))

(define-values
  (site-dispatcher site-url)
  (dispatch-rules
    [("") response-index]
    [("crew") response-crew]))

(serve/servlet
  site-dispatcher
  #:command-line? #t
  #:banner? #t
  #:servlet-regexp #rx""
  #:listen-ip "127.0.0.1"
  #:port 8000
  #:manager (create-none-manager response-not-found)
  #:server-root-path (current-directory)
  #:extra-files-paths (list "static")
  #:file-not-found-responder response-not-found)
