#lang racket/base

(provide
  task-page)

(require
  bonny/pirate
  "_base.rkt")

(define (task-page task)
  (base-page "Tasks" '()
    (lambda ()
      `(main
         (h2 "Task details for \"" ,(task-description task) "\"")
       ))))
