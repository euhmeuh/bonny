#lang racket/base

(require
  racket/function
  anaphoric
  "api.rkt")

(define (response-ok)
  (response #:code 200
            #:message #"OK"))

(define (response-unauthorized)
  (response #:code 401
            #:message #"Unauthorized"))

(define (response-webhook req)
  (define payload (get-payload req))
  (if (event-release? (get-event-type payload))
      (start-release (get-repo-url payload)
                     (get-commit-id payload))
      (response-unauthorized)))

(define (response-status [project-id #f])
  (aif (find-project project-id)
       (project-status it)
       (overall-status projects)))

(define (start-release repo-url commit-id)
  (define project (get-project-from-repo-url repo-url))
  (thread
    (thunk
      (if (stop-server project)
          (let ([repo (project-repo project)])
            (update-repo repo commit-id)
            (run-tests repo)
            (start-server project))
          (error 'server-unstoppable "Could not stop the server"))))
  (response-ok))

(struct project (repo [server #:mutable]))
(struct repo (url location))

(define (get-project-from-repo-url url))

(define (stop-server project))

(define (start-server project))

(define (update-repo repo commit-id)
  (define folder (repo-location repo))
  (cmd folder (format "git reset --hard ~a" commit-id)))

(define (run-tests repo)
  (cmd (repo-location repo)
       "raco test"))

(define (find-project project-id)
  (assq project-id projects))

(define projects (read "projects"))

(serve-api
  [("") response-unauthorized]
  [("webhook") response-webhook]
  [("status") response-status]
  [("status" (string-arg)) response-status])
