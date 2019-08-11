#lang racket/base

(require
  web-galaxy/response
  web-galaxy/serve
  web-galaxy/utils
  bonny/pirate
  "pages/_base.rkt"
  "pages/index.rkt"
  "pages/trails.rkt"
  "pages/logs.rkt")

(define (get-pirate id)
  (pirate #f
          "Random pirate"
          "https://random"
          0
          8000))

(define-response (index)
  (response/page (index-page)))

(define-response (trails pirate-id)
  (define pirate (get-pirate pirate-id))
  (response/page (trails-page pirate)))

(define-response (logs [pirate-id #f])
  (define pirate (and pirate-id (get-pirate pirate-id)))
  (response/page (logs-page pirate)))

(define-response (not-found)
  (response/page
    (base-page "404 Not found" '()
      (lambda ()
        '(main (p "404 Not found - The page you requested was not found."))))))

(parameterize ([current-server-static-paths (list "static")]
               [current-not-found-responder response-not-found]
               [current-server-port 8080])
  (serve/all
    [GET ("") response-index]
    [GET ("trails" (integer-arg)) response-trails]
    [GET ("logs") response-logs]
    [GET ("logs" (integer-arg)) response-logs]
    ))
