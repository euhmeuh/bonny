#lang racket/base

(require
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

(define-response (pirates)
  (response/json
    (list
      #hasheq([id . "0"] [name . "rilouw.eu"] [url . "https://github.com/euhmeuh/rilouw.eu"] [status . "RUNNING"])
      #hasheq([id . "1"] [name . "Rilouwiki"] [url . "https://github.com/euhmeuh/rilouwiki"] [status . "STARTING"])
      )))

(define-response (create-pirate)
  (define id 42)
  (response-parameterize ([Location (format "/pirate/~a" id)])
    (response/raw #:code 201 #:message #"Created" #"")))

(define-response (update-pirate id)
  (response/raw #:code 200 #:message #"Updated" #""))

(define-response (delete-pirate id)
  (response/raw #:code 200 #:message #"Deleted" #""))

(serve/all
  [GET ("") response-unauthorized]
  [GET ("webhook") response-webhook]
  [GET ("pirate") response-pirates]
  [POST ("pirate") response-create-pirate]
  [PUT ("pirate" (integer-arg)) response-update-pirate]
  [DELETE ("pirate" (integer-arg)) response-delete-pirate])
