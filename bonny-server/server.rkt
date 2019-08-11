#lang racket/base

(require
  web-galaxy/db
  web-galaxy/response
  web-galaxy/serve
  bonny/pirate
  db)


(define db (sqlite3-connect #:database (current-db-path)))

(define-response (unauthorized)
  (response/raw #:code 401
                #:message #"Unauthorized" #""))

(define-response (webhook)
  (response/raw #:code 200 #:message #"Webhook" #""))

(define-response (pirates)
  (define sort? (req-data "sort" req))
  (define dir? (req-data "direction" req))
  (response/json
    (get-all-pirates db
                     #:order-by (and sort? (string->symbol sort?))
                     #:direction (and dir? (string->symbol dir?)))))

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
