#lang racket/base

(require
  racket/function
  anaphoric
  web-galaxy/serve
  web-galaxy/response
  bonny/pirate
  bonny/utils)


(define-response (ok)
  (response/raw #:code 200
                #:message #"OK" #""))

(define-response (unauthorized)
  (response/raw #:code 401
                #:message #"Unauthorized" #""))

(define-response (webhook)
  (response/raw #:code 200 #:message #"Webhook" #""))
  ; (define payload (get-payload req))
  ; (if (event-release? (get-event-type payload))
  ;     (start-release (get-repository payload)
  ;                    (get-commit-id payload))
  ;     (response-unauthorized)))

(define-response (status [pirate-id #f])
  (response/raw #:code 200 #:message #"Status" #""))
  ; (aif (and pirate-id (find-pirate pirate-id))
  ;      (pirate-status it)
  ;      (overall-status pirates)))

(define-response (pirates)
  (response/json
    (list
      #hasheq([id . "0"] [name . "rilouw.eu"] [url . "https://github.com/euhmeuh/rilouw.eu"] [status . "RUNNING"])
      #hasheq([id . "1"] [name . "Rilouwiki"] [url . "https://github.com/euhmeuh/rilouwiki"] [status . "STARTING"])
      )))

; (define (start-release repository commit-id)
;   (define pirate (get-pirate-from-repository repository))
;   (thread
;     (thunk
;       (if (stop-server pirate)
;           (let ([repo (pirate-repo pirate)])
;             (update-repo repo commit-id)
;             (run-tests repo)
;             (start-server pirate))
;           (error 'server-unstoppable "Could not stop the server"))))
;   (response-ok))

(define (get-pirate-from-repository url)
  'TODO)

(define (stop-server pirate)
  'TODO)

(define (start-server pirate)
  'TODO)

; (define (update-repo repo commit-id)
;   (define folder (repo-location repo))
;   (cmd folder (format "git reset --hard ~a" commit-id)))

; (define (run-tests repo)
;   (cmd (repo-location repo)
;        "raco test"))

(serve/all
  [GET ("") response-unauthorized]
  [GET ("webhook") response-webhook]
  [GET ("pirates") response-pirates]
  [GET ("status") response-status]
  [GET ("status" (string-arg)) response-status])
