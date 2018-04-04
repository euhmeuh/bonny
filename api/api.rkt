#lang web-server

(provide
  serve-api
  server-root-path)

(require
  web-server/servlet
  web-server/servlet-env
  web-server/managers/none
  web-server/configuration/responders
  "../site-mode.rkt")

(define server-root-path (make-parameter (current-directory)))

(define (response-not-found)
  (response #:code 404
            #:message #"Not Found"))

(define (response-error url exception)
  (log-error "~s" `((exn ,(exn-message exception))
                    (uri ,(url->string url))
                    (time ,(current-seconds))))
  (response
    #:code 500
    #:message #"Internal server error"))

(define-syntax-rule (serve-api (route handler) ...)
  (let-values ((dispatcher url-maker)
               (dispath-rules (route handler) ...))
    (serve/servlet
      dispatcher
      #:command-line? #t
      #:banner? #t
      #:servlet-regexp #rx""
      #:listen-ip (if-debug "127.0.0.1" #f)
      #:port (if-debug 8000 80)
      #:manager (create-none-manager response-not-found)
      #:servlet-responder (if-debug servlet-error-responder response-error)
      #:server-root-path (server-root-path))))
