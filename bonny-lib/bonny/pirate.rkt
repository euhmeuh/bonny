#lang racket/base

(provide
  (struct-out pirate)
  (table-out pirate)
  git-url->directory
  get-all-pirates)

(require
  racket/contract
  racket/list
  racket/string
  web-galaxy/db
  web-galaxy/utils)

(define (git-url->directory url)
  (last (string-split url "/")))

(define status-enum '(STOPPED STARTING STARTED STOPPING))
(define (status? s) (memq s status-enum))
(define (integer->status i)
  (and (>= i 0)
       (< i (length status-enum))
       (list-ref status-enum i)))

(struct pirate (id name repository status port))

(define-table pirate
  (name text)
  (repository text)
  (status integer)
  (port integer))

(define (get-all-pirates db #:order-by [sort #f]
                            #:direction [dir #f])
  (define order-by (and (symbol? sort)
                        (cond/list
                          [_ 'order-by]
                          [_ sort]
                          [(or (eq? dir 'asc) (eq? dir 'desc)) dir])))
  (for/list ([p (if order-by
                    (db-list-pirate db order-by)
                    (db-list-pirate db))])
    (hash-update (make-immutable-hasheq p)
                 'status
                 (lambda (val)
                   (symbol->string (integer->status val))))))
