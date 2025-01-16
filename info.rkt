#lang info
(define collection "rrule")
(define deps '("gregor-lib"
               "rackunit-lib"
               "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/rrule.scrbl" ())))
(define pkg-desc "iCal recurrence rule implementation")
(define version "0.0")
(define pkg-authors '(benknoble))
(define license 'BSD-3-Clause)
