#lang racket/base

(provide
  overall-status
  project-status)

(define (overall-status projects)
  `(overall-status (projects ,projects)))

(define (project-status project)
  `(project-status ,project))
