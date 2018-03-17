#lang racket/base

(provide
  (struct-out pirate))

(struct pirate (name repo-url repo-name port))
