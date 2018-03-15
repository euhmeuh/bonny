#!/usr/bin/env racket
#lang racket/base

(require
  racket/string
  racket/function
  command-tree
  "call.rkt")

(define dependencies
  '("anaphoric"))

(command-tree
  `([install ,(thunk (call "raco pkg install --auto --skip-installed ~a" (string-join dependencies)))]
    [dev     ,(thunk (call "racket ./server.rkt"))]
    [run     ,(thunk (call "/usr/bin/env SITE_MODE=prod racket ./server.rkt"))]
    [test    ,(thunk (call "racket ./tests/test-all.rkt"))])
  (current-command-line-arguments))
