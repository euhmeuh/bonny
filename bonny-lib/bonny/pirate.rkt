#lang racket/base

(provide
  (struct-out pirate)
  git-url->directory)

(require
  racket/list
  racket/string)

(struct pirate (name repository port))

(define (git-url->directory url)
  (last (string-split url "/")))
