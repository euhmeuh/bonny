#lang racket/base

(provide
  define-cascader
  cascade)

(define-syntax-rule
  (define-cascader (name arg ...)
                   #:description desc
                   #:unless unless
                   #:fail fail
                   #:fail-reason reason
                   body ...)
  (define (name arg ...)
    (lambda ()
      (displayln desc)
      (cond
        [unless 'nothing-to-do]
        [fail (begin
                (displayln reason)
                'fail)]
        [else (begin body ...)]))))

(define (cascade . cascaders)
  (for/and ([cascader cascaders])
    (let ([result (cascader)])
      (if (eq? result 'fail)
          #f
          result))))
