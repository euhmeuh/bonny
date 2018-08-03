#lang info

(define collection 'multi)
(define deps '())
(define build-deps '("base"
                     "rackunit-lib"
                     "bonny-lib"))
(define update-implies '("bonny-lib"))
(define pkg-desc "Tests for \"bonny\"")
(define pkg-authors '(euhmeuh))
