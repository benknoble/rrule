#lang racket

(require gregor
         rackunit
         rrule)

(define-check (check-roundtrip-through-string a-rule dtstart)
  (define rule-from-str
    (parse-rrule (rrule->string a-rule)))
  (check-equal? (rrule->list rule-from-str dtstart)
                (rrule->list a-rule dtstart)))

(define-check (check-rrule-list a-rule dtstart moments)
  (check-equal? (rrule->list a-rule dtstart)
                moments))
