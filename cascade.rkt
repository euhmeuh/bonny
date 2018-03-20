#lang racket/base

(provide
  define-cascader
  cascade
  cascade-fail
  current-call-shell
  current-call-mode
  call)

(require
  racket/match
  racket/system
  (for-syntax
    racket/base
    syntax/parse))

(define current-call-shell (make-parameter #f))
(define current-call-mode (make-parameter 'normal))

(define (call #:dir [dir #f]
              command . args)
  (define cmd (apply format (cons command args)))
  (if (current-call-shell)
      ((current-call-shell) #:dir dir cmd)
      (parameterize ([current-directory (or dir (current-directory))])
        (displayln cmd)
        (unless (eq? (current-call-mode) 'dry)
          (system cmd #:set-pwd? #t)))))

(define-syntax (define-cascader stx)
  (define-splicing-syntax-class maybe-desc
    (pattern (~seq #:description desc:expr))
    (pattern (~seq) #:with desc #'#f))

  (define-splicing-syntax-class maybe-unless/when
    (pattern (~seq #:unless unless:expr))
    (pattern (~seq #:when when:expr)
             #:with unless #'(not when))
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
    [(define-cascader head:proc-head md:maybe-desc mu:maybe-unless/when mf:maybe-fail body ...)
     #'(define head
         (lambda ()
           (display "# ")
           (displayln (or md.desc 'head.name))
           (cond
             [mu.unless 'nothing-to-do]
             [mf.fail (displayln (or mf.reason "Command failed"))
                      'fail]
             [else body ...])))]))

(define (cascade . cascaders)
  (for/and ([cascader cascaders])
    (let ([result (cascader)])
      (match result
        ['fail #f]
        ['nothing-to-do
         (begin
           (displayln "Nothing to do")
           #t)]
        [_ result]))))

(define (cascade-fail message . args)
  (apply raise-user-error (cons 'cascade-failed
                                (cons message args))))
