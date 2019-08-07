#lang racket/base

(require
  command-tree)

(command-tree
  `([serve ,(lambda () (dynamic-require 'bonny-server/server #f))]
    [dashboard ,(lambda () (dynamic-require 'bonny-dashboard/server #f))])
  (current-command-line-arguments))
