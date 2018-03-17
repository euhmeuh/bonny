#lang racket/base

(provide
  define-cascader
  cascade)

(require
  (for-syntax
    racket/base
    syntax/parse))

(define-syntax (define-cascader stx)
  (define-splicing-syntax-class maybe-desc
    (pattern (~seq #:description desc:expr))
    (pattern (~seq) #:with desc #'#f))

  (define-splicing-syntax-class maybe-unless
    (pattern (~seq #:unless unless:expr))
    (pattern (~seq) #:with unless #'#f))

  (define-splicing-syntax-class maybe-fail
    (pattern (~seq #:fail fail:expr #:fail-reason reason:expr))
    (pattern (~seq #:fail fail:expr)
             #:with reason #'#f)
    (pattern (~seq)
             #:with fail #'#f
             #:with reason #'#f))

  (define-syntax-class proc-head
    (pattern (name arg ...))
    (pattern (name arg ... . rest-args)))

  (syntax-parse stx
    [(define-cascader head:proc-head md:maybe-desc mu:maybe-unless mf:maybe-fail body ...)
     #'(define head
         (lambda ()
           (displayln (or md.desc 'head.name))
           (cond
             [mu.unless 'nothing-to-do]
             [mf.fail (begin
                        (displayln (or mf.reason "Command failed"))
                        'fail)]
             [else (begin body ...)])))]))

(define (cascade . cascaders)
  (for/and ([cascader cascaders])
    (let ([result (cascader)])
      (if (eq? result 'fail)
          #f
          result))))
