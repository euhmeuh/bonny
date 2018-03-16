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

  (define (make-pattern stx proc-head desc unless fail reason body)
    (quasisyntax/loc stx
      (define #,proc-head
         (lambda ()
           (displayln (or #,desc 'name))
           (cond
             [#,unless 'nothing-to-do]
             [#,fail (begin
                        (displayln (or #,reason "Command failed"))
                        'fail)]
             [else (begin #,@body)])))))

  (syntax-parse stx
    [(define-cascader (name arg ...) md:maybe-desc mu:maybe-unless mf:maybe-fail body ...)
     (with-syntax ([proc-head #'(name arg ...)])
       (make-pattern stx #'proc-head #'md.desc #'mu.unless #'mf.fail #'mf.reason #'(body ...)))]
    ;; handle dotted pair
    [(define-cascader (name arg ... . rest-args) md:maybe-desc mu:maybe-unless mf:maybe-fail body ...)
     (with-syntax ([proc-head #'(name arg ... . rest-args)])
       (make-pattern stx #'proc-head #'md.desc #'mu.unless #'mf.fail #'mf.reason #'(body ...)))]))

(define (cascade . cascaders)
  (for/and ([cascader cascaders])
    (let ([result (cascader)])
      (if (eq? result 'fail)
          #f
          result))))
