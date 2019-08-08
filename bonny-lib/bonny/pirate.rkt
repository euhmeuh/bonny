#lang racket/base

(provide
  (struct-out pirate)
  make-pirate
  git-url->directory
  get-all-pirates)

(require
  racket/contract
  (only-in racket/list last)
  racket/string
  threading
  deta)

(define (git-url->directory url)
  (last (string-split url "/")))

(define status-enum '(STOPPED STARTING STARTED STOPPING))
(define (status? s) (memq s status-enum))
(define (integer->status i)
  (and (>= i 0)
       (< i (length status-enum))
       (list-ref status-enum i)))

(define-schema pirate
  ([id id/f #:primary-key #:auto-increment]
   [name string/f #:contract non-empty-string?]
   [repository string/f #:contract non-empty-string?]
   [status integer/f #:contract integer->status]
   [port integer/f #:contract (>=/c 8000)]))

(define (get-all-pirates db #:order-by [sort #f]
                            #:direction [dir #f])
  (for/list ([p (in-entities db (~> (from pirate #:as p)
                                    (order-by ([p.name #:desc]))))])
    (hasheq
      'id (pirate-id p)
      'name (pirate-name p)
      'url (pirate-repository p)
      'status (symbol->string (integer->status (pirate-status p)))
      'port (pirate-port p))))
