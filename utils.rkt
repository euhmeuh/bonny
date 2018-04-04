#lang racket/base

(require
  racket/contract/base)

(provide/contract
  [read-sexp-file (-> path-string? any/c)])

(provide
  cond/list
  cond/string)

(require
  racket/string
  (for-syntax
    racket/base
    syntax/parse))

(begin-for-syntax
  (define-syntax-class maybe-cond
    #:literals (_)
    (pattern (_ value:expr)
      #:with condition #'#t)
    (pattern (condition:expr value:expr))))

(define-syntax (cond/list stx)
  (syntax-parse stx
    [(cond/list mc:maybe-cond ...)
     #'(let* ([result null]
              [result (if mc.condition
                          (cons mc.value result)
                          result)] ...)
         (reverse result))]))

(define-syntax (cond/string stx)
  (define-splicing-syntax-class maybe-sep
    (pattern (~seq #:separator sep:str))
    (pattern (~seq) #:with sep #'" "))
  (syntax-parse stx
    [(_ mc:maybe-cond ... ms:maybe-sep join-options ...)
     #'(string-join (cond/list mc ...) ms.sep join-options ...)]))

(define (read-sexp-file sexp-file)
  (call-with-input-file sexp-file
    (lambda (in)
      (call-with-default-reading-parameterization
        (lambda () (read in))))))

(module+ test
  (require
    rackunit)

  (define beer 'la-chouffe)
  (define (fresh? beer) #t)
  (define (open-bottle beer) 'opened-beer)
  (define time 'afternoon)

  (define things-i-like
    (cond/list
      [_ 'carpaccio]
      [_ 'pasta]
      [(eq? time 'morning) 'croissant]
      [(eq? time 'afternoon) 'brioche]
      [#f 'salsifi]
      [(fresh? beer) (open-bottle beer)]))

  (check-equal? things-i-like
                '(carpaccio pasta brioche opened-beer))

  (check-equal? (cond/string
                  [_ "I really"]
                  [_ "love"]
                  [#f "hate"]
                  [_ "food"]
                  #:separator " * "
                  #:after-last "!")
                "I really * love * food!")

  )
