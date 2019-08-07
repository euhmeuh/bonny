#lang racket/base

(provide
  (except-out (struct-out datagrid) datagrid)
  (rename-out [make-datagrid datagrid])
  render-datagrid
  datagrid-script)

(require
  (for-syntax
    racket/base
    syntax/parse)
  syntax/parse
  syntax/parse/define
  web-galaxy/renderer
  web-galaxy/javascript)

(define datagrid-script
  (javascript
    (define (datagrid-ctrl id resource)
      (= this.id id)
      (this.load resource))
    (define (load resource)
      (console.log resource))))

(define-renderer datagrid (id resource columns header footer)
  `(table ([id ,id])
     (thead)
     (tbody)
     (tfoot)))

(begin-for-syntax
  (define-splicing-syntax-class maybe-header
    #:datum-literals (header)
    (pattern (header xexpr ...) #:with headers #''(xexpr ...))
    (pattern (~seq) #:with headers #''()))

  (define-splicing-syntax-class maybe-footer
    #:datum-literals (footer)
    (pattern (footer xexpr ...) #:with footers #''(xexpr ...))
    (pattern (~seq) #:with footers #''()))

  (define-splicing-syntax-class maybe-name
    (pattern (~seq #:name name:string))
    (pattern (~seq) #:with name #'""))

  (define-splicing-syntax-class maybe-value
    (pattern (~seq #:value value:string))
    (pattern (~seq) #:with value #'#f))

  (define-syntax-class column
    #:datum-literals (column)
    (pattern (column mn:maybe-name mv:maybe-value xexpr ...)))
  )

(define-simple-macro
  (make-datagrid resource:string
                 mh:maybe-header
                 col:column ...
                 mf:maybe-footer)
  (datagrid "datagrid1" resource '(col ...) mh.headers mf.footers))

(module+ test
  (require rackunit)

  (test-case "Datagrid with no columns"
    (check-equal? (render-element (make-datagrid "/pirate"))
                  '(table ([id "datagrid1"])
                     (thead)
                     (tbody)
                     (tfoot)))))
