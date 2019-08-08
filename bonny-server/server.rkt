#lang racket/base

(require
  web-galaxy/db
  web-galaxy/serve
  web-galaxy/response
  bonny/pirate
  bonny/utils
  db)


(define db (sqlite3-connect #:database (current-db-path)))

(define-response (unauthorized)
  (response/raw #:code 401
                #:message #"Unauthorized" #""))

(define-response (webhook)
  (response/raw #:code 200 #:message #"Webhook" #""))

(define-response (pirates)
  (response/json
    (get-all-pirates db
                     #:order-by (req-data "sort" req)
                     #:direction (req-data "direction" req))))

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
