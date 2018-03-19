#lang racket/base

(provide
  cond/list)

(require
  (for-syntax
    racket/base
    syntax/parse))

(define-syntax (cond/list stx)

  (define-syntax-class maybe-cond
    #:literals (_)
    (pattern (_ value:expr)
      #:with condition #'#t)
    (pattern (condition:expr value:expr)))

  (syntax-parse stx
    [(cond/list mc:maybe-cond ...)
     #'(let* ([result null]
              [result (if mc.condition
                          (cons mc.value result)
                          result)] ...)
         (reverse result))]))

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

  )
