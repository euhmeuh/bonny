#lang web-server

(require
  racket/function)

(define (response-ok)
  (response 200 #"OK"))

(define (response-unauthorized)
  (response 401 #"Unauthorized"))

(define (response-webhook req)
  (define payload (get-payload req))
  (if (event-release? (get-event-type payload))
      (start-release (get-repo-url payload)
                     (get-commit-id payload))
      (response-unauthorized)))

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

(serve-api
  ("" response-unauthorized)
  ("webhook" response-webhook))
