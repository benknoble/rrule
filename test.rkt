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

(test-case "test-yearly"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1998 9 2 9 0)
                          (moment 1999 9 2 9 0))))

(test-case "test-yearly-interval"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:interval 2)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1999 9 2 9 0)
                          (moment 2001 9 2 9 0))))

(test-case "test-yearly-interval-large"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:interval 100)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 2097 9 2 9 0)
                          (moment 2197 9 2 9 0))))

(test-case "test-yearly-by-month"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:bymonth '(1 3))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 2 9 0)
                          (moment 1998 3 2 9 0)
                          (moment 1999 1 2 9 0))))

(test-case "test-yearly-by-month-day"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:bymonthday '(1 3))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 3 9 0)
                          (moment 1997 10 1 9 0)
                          (moment 1997 10 3 9 0))))

(test-case "test-yearly-by-month-and-month-day"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:bymonth '(1 3)
                                #:bymonthday '(5 7))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 5 9 0)
                          (moment 1998 1 7 9 0)
                          (moment 1998 3 5 9 0))))

(test-case "test-yearly-by-week-day"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 4 9 0)
                          (moment 1997 9 9 9 0))))

(test-case "test-yearly-by-n-week-day"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:byday '((1 tuesday) (-1 thursday)))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 25 9 0)
                          (moment 1998 1 6 9 0)
                          (moment 1998 12 31 9 0))))

(test-case "test-yearly-by-n-week-day-large"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:byday '((3 tuesday) (-3 thursday)))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 11 9 0)
                          (moment 1998 1 20 9 0)
                          (moment 1998 12 17 9 0))))

(test-case "test-yearly-by-month-and-week-day"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:bymonth '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 9 0)
                          (moment 1998 1 6 9 0)
                          (moment 1998 1 8 9 0))))

(test-case "test-yearly-by-month-and-n-week-day"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:bymonth '(1 3)
                                #:byday '((1 tuesday) (-1 thursday)))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 6 9 0)
                          (moment 1998 1 29 9 0)
                          (moment 1998 3 3 9 0))))

(test-case "test-yearly-by-month-and-n-week-day-large"
  ;; This is interesting because the TH(-3) ends up before the TU(3).
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:bymonth '(1 3)
                                #:byday '((3 tuesday) (-3 thursday)))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 15 9 0)
                          (moment 1998 1 20 9 0)
                          (moment 1998 3 12 9 0))))

(test-case "test-yearly-by-month-day-and-week-day"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:bymonthday '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 9 0)
                          (moment 1998 2 3 9 0)
                          (moment 1998 3 3 9 0))))

(test-case "test-yearly-by-month-and-month-day-and-week-day"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:bymonth '(1 3)
                                #:bymonthday '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 9 0)
                          (moment 1998 3 3 9 0)
                          (moment 2001 3 1 9 0))))

(test-case "test-yearly-by-year-day"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 4
                                #:byyearday '(1 100 200 365))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 31 9 0)
                          (moment 1998 1 1 9 0)
                          (moment 1998 4 10 9 0)
                          (moment 1998 7 19 9 0))))

(test-case "test-yearly-by-year-day-neg"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 4
                                #:byyearday '(-365 -266 -166 -1))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 31 9 0)
                          (moment 1998 1 1 9 0)
                          (moment 1998 4 10 9 0)
                          (moment 1998 7 19 9 0))))

(test-case "test-yearly-by-month-and-year-day"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 4
                                #:bymonth '(4 7)
                                #:byyearday '(1 100 200 365))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 4 10 9 0)
                          (moment 1998 7 19 9 0)
                          (moment 1999 4 10 9 0)
                          (moment 1999 7 19 9 0))))

(test-case "test-yearly-by-month-and-year-day-neg"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 4
                                #:bymonth '(4 7)
                                #:byyearday '(-365 -266 -166 -1))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 4 10 9 0)
                          (moment 1998 7 19 9 0)
                          (moment 1999 4 10 9 0)
                          (moment 1999 7 19 9 0))))

(test-case "test-yearly-by-week-no"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:byweeknumber '(20))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 5 11 9 0)
                          (moment 1998 5 12 9 0)
                          (moment 1998 5 13 9 0))))

(test-case "test-yearly-by-week-no-and-week-day"
  ;; That's a nice one. The first days of week number one may be in the last
  ;; year.
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:byweeknumber '(1)
                                #:byday '(monday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 29 9 0)
                          (moment 1999 1 4 9 0)
                          (moment 2000 1 3 9 0))))

(test-case "test-yearly-by-week-no-and-week-day-large"
  ;; Another nice test. The last days of week number 52/53 may be in the next
  ;; year.
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:byweeknumber '(52)
                                #:byday '(sunday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 28 9 0)
                          (moment 1998 12 27 9 0)
                          (moment 2000 1 2 9 0))))

(test-case "test-yearly-by-week-no-and-week-day-last"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:byweeknumber '(-1)
                                #:byday '(sunday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 28 9 0)
                          (moment 1999 1 3 9 0)
                          (moment 2000 1 2 9 0))))

(test-case "test-yearly-by-week-no-and-week-day53"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:byweeknumber '(53)
                                #:byday '(monday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 12 28 9 0)
                          (moment 2004 12 27 9 0)
                          (moment 2009 12 28 9 0))))

(test-case "test-yearly-by-hour"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:byhour '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 0)
                          (moment 1998 9 2 6 0)
                          (moment 1998 9 2 18 0))))

(test-case "test-yearly-by-minute"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:byminute '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 6)
                          (moment 1997 9 2 9 18)
                          (moment 1998 9 2 9 6))))

(test-case "test-yearly-by-second"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0 6)
                          (moment 1997 9 2 9 0 18)
                          (moment 1998 9 2 9 0 6))))

(test-case "test-yearly-by-hour-and-minute"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:byhour '(6 18)
                                #:byminute '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 6)
                          (moment 1997 9 2 18 18)
                          (moment 1998 9 2 6 6))))

(test-case "test-yearly-by-hour-and-second"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:byhour '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 0 6)
                          (moment 1997 9 2 18 0 18)
                          (moment 1998 9 2 6 0 6))))

(test-case "test-yearly-by-minute-and-second"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:byminute '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 6 6)
                          (moment 1997 9 2 9 6 18)
                          (moment 1997 9 2 9 18 6))))

(test-case "test-yearly-by-hour-and-minute-and-second"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:byhour '(6 18)
                                #:byminute '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 6 6)
                          (moment 1997 9 2 18 6 18)
                          (moment 1997 9 2 18 18 6))))

(test-case "test-yearly-by-set-pos"
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 3
                                #:bymonthday '(15)
                                #:byhour '(6 18)
                                #:bysetpos '(3 -3))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 11 15 18 0)
                          (moment 1998 2 15 6 0)
                          (moment 1998 11 15 18 0))))

(test-case "test-monthly"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 10 2 9 0)
                          (moment 1997 11 2 9 0))))

(test-case "test-monthly-interval"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:interval 2)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 11 2 9 0)
                          (moment 1998 1 2 9 0))))

(test-case "test-monthly-interval-large"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:interval 18)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1999 3 2 9 0)
                          (moment 2000 9 2 9 0))))

(test-case "test-monthly-by-month"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:bymonth '(1 3))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 2 9 0)
                          (moment 1998 3 2 9 0)
                          (moment 1999 1 2 9 0))))

(test-case "test-monthly-by-month-day"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:bymonthday '(1 3))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 3 9 0)
                          (moment 1997 10 1 9 0)
                          (moment 1997 10 3 9 0))))

(test-case "test-monthly-by-month-and-month-day"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:bymonth '(1 3)
                                #:bymonthday '(5 7))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 5 9 0)
                          (moment 1998 1 7 9 0)
                          (moment 1998 3 5 9 0))))

(test-case "test-monthly-by-week-day"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 4 9 0)
                          (moment 1997 9 9 9 0)))

  ;; Third Monday of the month
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:byday '((+3 monday)))
                    (moment 1997 9 1)
                    (list (moment 1997 9 15 0 0)
                          (moment 1997 10 20 0 0)
                          (moment 1997 11 17 0 0))))

(test-case "test-monthly-by-n-week-day"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:byday '((1 tuesday) (-1 thursday)))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 25 9 0)
                          (moment 1997 10 7 9 0))))

(test-case "test-monthly-by-n-week-day-large"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:byday '((3 tuesday) (-3 thursday)))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 11 9 0)
                          (moment 1997 9 16 9 0)
                          (moment 1997 10 16 9 0))))

(test-case "test-monthly-by-month-and-week-day"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:bymonth '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 9 0)
                          (moment 1998 1 6 9 0)
                          (moment 1998 1 8 9 0))))

(test-case "test-monthly-by-month-and-n-week-day"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:bymonth '(1 3)
                                #:byday '((1 tuesday) (-1 thursday)))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 6 9 0)
                          (moment 1998 1 29 9 0)
                          (moment 1998 3 3 9 0))))

(test-case "test-monthly-by-month-and-n-week-day-large"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:bymonth '(1 3)
                                #:byday '((3 tuesday) (-3 thursday)))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 15 9 0)
                          (moment 1998 1 20 9 0)
                          (moment 1998 3 12 9 0))))

(test-case "test-monthly-by-month-day-and-week-day"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:bymonthday '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 9 0)
                          (moment 1998 2 3 9 0)
                          (moment 1998 3 3 9 0))))

(test-case "test-monthly-by-month-and-month-day-and-week-day"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:bymonth '(1 3)
                                #:bymonthday '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 9 0)
                          (moment 1998 3 3 9 0)
                          (moment 2001 3 1 9 0))))

(test-case "test-monthly-by-year-day"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 4
                                #:byyearday '(1 100 200 365))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 31 9 0)
                          (moment 1998 1 1 9 0)
                          (moment 1998 4 10 9 0)
                          (moment 1998 7 19 9 0))))

(test-case "test-monthly-by-year-day-neg"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 4
                                #:byyearday '(-365 -266 -166 -1))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 31 9 0)
                          (moment 1998 1 1 9 0)
                          (moment 1998 4 10 9 0)
                          (moment 1998 7 19 9 0))))

(test-case "test-monthly-by-month-and-year-day"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 4
                                #:bymonth '(4 7)
                                #:byyearday '(1 100 200 365))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 4 10 9 0)
                          (moment 1998 7 19 9 0)
                          (moment 1999 4 10 9 0)
                          (moment 1999 7 19 9 0))))

(test-case "test-monthly-by-month-and-year-day-neg"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 4
                                #:bymonth '(4 7)
                                #:byyearday '(-365 -266 -166 -1))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 4 10 9 0)
                          (moment 1998 7 19 9 0)
                          (moment 1999 4 10 9 0)
                          (moment 1999 7 19 9 0))))

(test-case "test-monthly-by-week-no"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:byweeknumber '(20))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 5 11 9 0)
                          (moment 1998 5 12 9 0)
                          (moment 1998 5 13 9 0))))

(test-case "test-monthly-by-week-no-and-week-day"
  ;; That's a nice one. The first days of week number one may be in the last
  ;; year.
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:byweeknumber '(1)
                                #:byday '(monday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 29 9 0)
                          (moment 1999 1 4 9 0)
                          (moment 2000 1 3 9 0))))

(test-case "test-monthly-by-week-no-and-week-day-large"
  ;; Another nice test. The last days of week number 52/53 may be in the next
  ;; year.
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:byweeknumber '(52)
                                #:byday '(sunday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 28 9 0)
                          (moment 1998 12 27 9 0)
                          (moment 2000 1 2 9 0))))

(test-case "test-monthly-by-week-no-and-week-day-last"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:byweeknumber '(-1)
                                #:byday '(sunday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 28 9 0)
                          (moment 1999 1 3 9 0)
                          (moment 2000 1 2 9 0))))

(test-case "test-monthly-by-week-no-and-week-day53"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:byweeknumber '(53)
                                #:byday '(monday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 12 28 9 0)
                          (moment 2004 12 27 9 0)
                          (moment 2009 12 28 9 0))))

(test-case "test-monthly-by-hour"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:byhour '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 0)
                          (moment 1997 10 2 6 0)
                          (moment 1997 10 2 18 0))))

(test-case "test-monthly-by-minute"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:byminute '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 6)
                          (moment 1997 9 2 9 18)
                          (moment 1997 10 2 9 6))))

(test-case "test-monthly-by-second"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0 6)
                          (moment 1997 9 2 9 0 18)
                          (moment 1997 10 2 9 0 6))))

(test-case "test-monthly-by-hour-and-minute"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:byhour '(6 18)
                                #:byminute '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 6)
                          (moment 1997 9 2 18 18)
                          (moment 1997 10 2 6 6))))

(test-case "test-monthly-by-hour-and-second"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:byhour '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 0 6)
                          (moment 1997 9 2 18 0 18)
                          (moment 1997 10 2 6 0 6))))

(test-case "test-monthly-by-minute-and-second"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:byminute '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 6 6)
                          (moment 1997 9 2 9 6 18)
                          (moment 1997 9 2 9 18 6))))

(test-case "test-monthly-by-hour-and-minute-and-second"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:byhour '(6 18)
                                #:byminute '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 6 6)
                          (moment 1997 9 2 18 6 18)
                          (moment 1997 9 2 18 18 6))))

(test-case "test-monthly-by-set-pos"
  (check-rrule-list (make-rrule #:freq 'monthly
                                #:count 3
                                #:bymonthday '(13 17)
                                #:byhour '(6 18)
                                #:bysetpos '(3 -3))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 13 18 0)
                          (moment 1997 9 17 6 0)
                          (moment 1997 10 13 18 0))))

(test-case "test-weekly"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 9 9 0)
                          (moment 1997 9 16 9 0))))

(test-case "test-weekly-interval"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:interval 2)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 16 9 0)
                          (moment 1997 9 30 9 0))))

(test-case "test-weekly-interval-large"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:interval 20)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1998 1 20 9 0)
                          (moment 1998 6 9 9 0))))

(test-case "test-weekly-by-month"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:bymonth '(1 3))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 6 9 0)
                          (moment 1998 1 13 9 0)
                          (moment 1998 1 20 9 0))))

(test-case "test-weekly-by-month-day"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:bymonthday '(1 3))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 3 9 0)
                          (moment 1997 10 1 9 0)
                          (moment 1997 10 3 9 0))))

(test-case "test-weekly-by-month-and-month-day"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:bymonth '(1 3)
                                #:bymonthday '(5 7))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 5 9 0)
                          (moment 1998 1 7 9 0)
                          (moment 1998 3 5 9 0))))

(test-case "test-weekly-by-week-day"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 4 9 0)
                          (moment 1997 9 9 9 0))))

(test-case "test-weekly-by-n-week-day"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:byday '((1 tuesday) (-1 thursday)))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 4 9 0)
                          (moment 1997 9 9 9 0))))

(test-case "test-weekly-by-month-and-week-day"
  ;; This test is interesting because it crosses the year boundary in a weekly
  ;; period to find day '1' as a valid recurrence.
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:bymonth '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 9 0)
                          (moment 1998 1 6 9 0)
                          (moment 1998 1 8 9 0))))

(test-case "test-weekly-by-month-and-n-week-day"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:bymonth '(1 3)
                                #:byday '((1 tuesday) (-1 thursday)))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 9 0)
                          (moment 1998 1 6 9 0)
                          (moment 1998 1 8 9 0))))

(test-case "test-weekly-by-month-day-and-week-day"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:bymonthday '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 9 0)
                          (moment 1998 2 3 9 0)
                          (moment 1998 3 3 9 0))))

(test-case "test-weekly-by-month-and-month-day-and-week-day"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:bymonth '(1 3)
                                #:bymonthday '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 9 0)
                          (moment 1998 3 3 9 0)
                          (moment 2001 3 1 9 0))))

(test-case "test-weekly-by-year-day"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 4
                                #:byyearday '(1 100 200 365))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 31 9 0)
                          (moment 1998 1 1 9 0)
                          (moment 1998 4 10 9 0)
                          (moment 1998 7 19 9 0))))

(test-case "test-weekly-by-year-day-neg"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 4
                                #:byyearday '(-365 -266 -166 -1))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 31 9 0)
                          (moment 1998 1 1 9 0)
                          (moment 1998 4 10 9 0)
                          (moment 1998 7 19 9 0))))

(test-case "test-weekly-by-month-and-year-day"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 4
                                #:bymonth '(1 7)
                                #:byyearday '(1 100 200 365))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 9 0)
                          (moment 1998 7 19 9 0)
                          (moment 1999 1 1 9 0)
                          (moment 1999 7 19 9 0))))

(test-case "test-weekly-by-month-and-year-day-neg"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 4
                                #:bymonth '(1 7)
                                #:byyearday '(-365 -266 -166 -1))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 9 0)
                          (moment 1998 7 19 9 0)
                          (moment 1999 1 1 9 0)
                          (moment 1999 7 19 9 0))))

(test-case "test-weekly-by-week-no"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:byweeknumber '(20))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 5 11 9 0)
                          (moment 1998 5 12 9 0)
                          (moment 1998 5 13 9 0))))

(test-case "test-weekly-by-week-no-and-week-day"
  ;; That's a nice one. The first days of week number one may be in the last
  ;; year.
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:byweeknumber '(1)
                                #:byday '(monday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 29 9 0)
                          (moment 1999 1 4 9 0)
                          (moment 2000 1 3 9 0))))

(test-case "test-weekly-by-week-no-and-week-day-large"
  ;; Another nice test. The last days of week number 52/53 may be in the next
  ;; year.
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:byweeknumber '(52)
                                #:byday '(sunday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 28 9 0)
                          (moment 1998 12 27 9 0)
                          (moment 2000 1 2 9 0))))

(test-case "test-weekly-by-week-no-and-week-day-last"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:byweeknumber '(-1)
                                #:byday '(sunday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 28 9 0)
                          (moment 1999 1 3 9 0)
                          (moment 2000 1 2 9 0))))

(test-case "test-weekly-by-week-no-and-week-day53"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:byweeknumber '(53)
                                #:byday '(monday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 12 28 9 0)
                          (moment 2004 12 27 9 0)
                          (moment 2009 12 28 9 0))))

(test-case "test-weekly-by-hour"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:byhour '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 0)
                          (moment 1997 9 9 6 0)
                          (moment 1997 9 9 18 0))))

(test-case "test-weekly-by-minute"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:byminute '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 6)
                          (moment 1997 9 2 9 18)
                          (moment 1997 9 9 9 6))))

(test-case "test-weekly-by-second"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0 6)
                          (moment 1997 9 2 9 0 18)
                          (moment 1997 9 9 9 0 6))))

(test-case "test-weekly-by-hour-and-minute"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:byhour '(6 18)
                                #:byminute '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 6)
                          (moment 1997 9 2 18 18)
                          (moment 1997 9 9 6 6))))

(test-case "test-weekly-by-hour-and-second"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:byhour '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 0 6)
                          (moment 1997 9 2 18 0 18)
                          (moment 1997 9 9 6 0 6))))

(test-case "test-weekly-by-minute-and-second"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:byminute '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 6 6)
                          (moment 1997 9 2 9 6 18)
                          (moment 1997 9 2 9 18 6))))

(test-case "test-weekly-by-hour-and-minute-and-second"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:byhour '(6 18)
                                #:byminute '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 6 6)
                          (moment 1997 9 2 18 6 18)
                          (moment 1997 9 2 18 18 6))))

(test-case "test-weekly-by-set-pos"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:byday '(tuesday thursday)
                                #:byhour '(6 18)
                                #:bysetpos '(3 -3))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 0)
                          (moment 1997 9 4 6 0)
                          (moment 1997 9 9 18 0))))

(test-case "test-daily"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 3 9 0)
                          (moment 1997 9 4 9 0))))

(test-case "test-daily-interval"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:interval 2)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 4 9 0)
                          (moment 1997 9 6 9 0))))

(test-case "test-daily-interval-large"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:interval 92)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 12 3 9 0)
                          (moment 1998 3 5 9 0))))

(test-case "test-daily-by-month"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:bymonth '(1 3))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 9 0)
                          (moment 1998 1 2 9 0)
                          (moment 1998 1 3 9 0))))

(test-case "test-daily-by-month-day"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:bymonthday '(1 3))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 3 9 0)
                          (moment 1997 10 1 9 0)
                          (moment 1997 10 3 9 0))))

(test-case "test-daily-by-month-and-month-day"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:bymonth '(1 3)
                                #:bymonthday '(5 7))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 5 9 0)
                          (moment 1998 1 7 9 0)
                          (moment 1998 3 5 9 0))))

(test-case "test-daily-by-week-day"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 4 9 0)
                          (moment 1997 9 9 9 0))))

(test-case "test-daily-by-n-week-day"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:byday '((1 tuesday) (-1 thursday)))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 4 9 0)
                          (moment 1997 9 9 9 0))))

(test-case "test-daily-by-month-and-week-day"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:bymonth '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 9 0)
                          (moment 1998 1 6 9 0)
                          (moment 1998 1 8 9 0))))

(test-case "test-daily-by-month-and-n-week-day"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:bymonth '(1 3)
                                #:byday '((1 tuesday) (-1 thursday)))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 9 0)
                          (moment 1998 1 6 9 0)
                          (moment 1998 1 8 9 0))))

(test-case "test-daily-by-month-day-and-week-day"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:bymonthday '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 9 0)
                          (moment 1998 2 3 9 0)
                          (moment 1998 3 3 9 0))))

(test-case "test-daily-by-month-and-month-day-and-week-day"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:bymonth '(1 3)
                                #:bymonthday '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 9 0)
                          (moment 1998 3 3 9 0)
                          (moment 2001 3 1 9 0))))

(test-case "test-daily-by-year-day"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 4
                                #:byyearday '(1 100 200 365))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 31 9 0)
                          (moment 1998 1 1 9 0)
                          (moment 1998 4 10 9 0)
                          (moment 1998 7 19 9 0))))

(test-case "test-daily-by-year-day-neg"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 4
                                #:byyearday '(-365 -266 -166 -1))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 31 9 0)
                          (moment 1998 1 1 9 0)
                          (moment 1998 4 10 9 0)
                          (moment 1998 7 19 9 0))))

(test-case "test-daily-by-month-and-year-day"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 4
                                #:bymonth '(1 7)
                                #:byyearday '(1 100 200 365))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 9 0)
                          (moment 1998 7 19 9 0)
                          (moment 1999 1 1 9 0)
                          (moment 1999 7 19 9 0))))

(test-case "test-daily-by-month-and-year-day-neg"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 4
                                #:bymonth '(1 7)
                                #:byyearday '(-365 -266 -166 -1))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 9 0)
                          (moment 1998 7 19 9 0)
                          (moment 1999 1 1 9 0)
                          (moment 1999 7 19 9 0))))

(test-case "test-daily-by-week-no"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:byweeknumber '(20))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 5 11 9 0)
                          (moment 1998 5 12 9 0)
                          (moment 1998 5 13 9 0))))

(test-case "test-daily-by-week-no-and-week-day"
  ;; That's a nice one. The first days of week number one may be in the last
  ;; year.
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:byweeknumber '(1)
                                #:byday '(monday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 29 9 0)
                          (moment 1999 1 4 9 0)
                          (moment 2000 1 3 9 0))))

(test-case "test-daily-by-week-no-and-week-day-large"
  ;; Another nice test. The last days of week number 52/53 may be in the next
  ;; year.
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:byweeknumber '(52)
                                #:byday '(sunday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 28 9 0)
                          (moment 1998 12 27 9 0)
                          (moment 2000 1 2 9 0))))

(test-case "test-daily-by-week-no-and-week-day-last"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:byweeknumber '(-1)
                                #:byday '(sunday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 28 9 0)
                          (moment 1999 1 3 9 0)
                          (moment 2000 1 2 9 0))))

(test-case "test-daily-by-week-no-and-week-day53"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:byweeknumber '(53)
                                #:byday '(monday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 12 28 9 0)
                          (moment 2004 12 27 9 0)
                          (moment 2009 12 28 9 0))))

(test-case "test-daily-by-hour"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:byhour '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 0)
                          (moment 1997 9 3 6 0)
                          (moment 1997 9 3 18 0))))

(test-case "test-daily-by-minute"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:byminute '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 6)
                          (moment 1997 9 2 9 18)
                          (moment 1997 9 3 9 6))))

(test-case "test-daily-by-second"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0 6)
                          (moment 1997 9 2 9 0 18)
                          (moment 1997 9 3 9 0 6))))

(test-case "test-daily-by-hour-and-minute"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:byhour '(6 18)
                                #:byminute '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 6)
                          (moment 1997 9 2 18 18)
                          (moment 1997 9 3 6 6))))

(test-case "test-daily-by-hour-and-second"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:byhour '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 0 6)
                          (moment 1997 9 2 18 0 18)
                          (moment 1997 9 3 6 0 6))))

(test-case "test-daily-by-minute-and-second"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:byminute '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 6 6)
                          (moment 1997 9 2 9 6 18)
                          (moment 1997 9 2 9 18 6))))

(test-case "test-daily-by-hour-and-minute-and-second"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:byhour '(6 18)
                                #:byminute '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 6 6)
                          (moment 1997 9 2 18 6 18)
                          (moment 1997 9 2 18 18 6))))

(test-case "test-daily-by-set-pos"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3
                                #:byhour '(6 18)
                                #:byminute '(15 45)
                                #:bysetpos '(3 -3))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 15)
                          (moment 1997 9 3 6 45)
                          (moment 1997 9 3 18 15))))

(test-case "test-hourly"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 2 10 0)
                          (moment 1997 9 2 11 0))))

(test-case "test-hourly-interval"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:interval 2)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 2 11 0)
                          (moment 1997 9 2 13 0))))

(test-case "test-hourly-interval-large"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:interval 769)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 10 4 10 0)
                          (moment 1997 11 5 11 0))))

(test-case "test-hourly-by-month"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:bymonth '(1 3))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 0 0)
                          (moment 1998 1 1 1 0)
                          (moment 1998 1 1 2 0))))

(test-case "test-hourly-by-month-day"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:bymonthday '(1 3))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 3 0 0)
                          (moment 1997 9 3 1 0)
                          (moment 1997 9 3 2 0))))

(test-case "test-hourly-by-month-and-month-day"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:bymonth '(1 3)
                                #:bymonthday '(5 7))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 5 0 0)
                          (moment 1998 1 5 1 0)
                          (moment 1998 1 5 2 0))))

(test-case "test-hourly-by-week-day"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 2 10 0)
                          (moment 1997 9 2 11 0))))

(test-case "test-hourly-by-n-week-day"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:byday '((1 tuesday) (-1 thursday)))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 2 10 0)
                          (moment 1997 9 2 11 0))))

(test-case "test-hourly-by-month-and-week-day"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:bymonth '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 0 0)
                          (moment 1998 1 1 1 0)
                          (moment 1998 1 1 2 0))))

(test-case "test-hourly-by-month-and-n-week-day"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:bymonth '(1 3)
                                #:byday '((1 tuesday) (-1 thursday)))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 0 0)
                          (moment 1998 1 1 1 0)
                          (moment 1998 1 1 2 0))))

(test-case "test-hourly-by-month-day-and-week-day"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:bymonthday '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 0 0)
                          (moment 1998 1 1 1 0)
                          (moment 1998 1 1 2 0))))

(test-case "test-hourly-by-month-and-month-day-and-week-day"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:bymonth '(1 3)
                                #:bymonthday '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 0 0)
                          (moment 1998 1 1 1 0)
                          (moment 1998 1 1 2 0))))

(test-case "test-hourly-by-year-day"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 4
                                #:byyearday '(1 100 200 365))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 31 0 0)
                          (moment 1997 12 31 1 0)
                          (moment 1997 12 31 2 0)
                          (moment 1997 12 31 3 0))))

(test-case "test-hourly-by-year-day-neg"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 4
                                #:byyearday '(-365 -266 -166 -1))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 31 0 0)
                          (moment 1997 12 31 1 0)
                          (moment 1997 12 31 2 0)
                          (moment 1997 12 31 3 0))))

(test-case "test-hourly-by-month-and-year-day"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 4
                                #:bymonth '(4 7)
                                #:byyearday '(1 100 200 365))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 4 10 0 0)
                          (moment 1998 4 10 1 0)
                          (moment 1998 4 10 2 0)
                          (moment 1998 4 10 3 0))))

(test-case "test-hourly-by-month-and-year-day-neg"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 4
                                #:bymonth '(4 7)
                                #:byyearday '(-365 -266 -166 -1))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 4 10 0 0)
                          (moment 1998 4 10 1 0)
                          (moment 1998 4 10 2 0)
                          (moment 1998 4 10 3 0))))

(test-case "test-hourly-by-week-no"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:byweeknumber '(20))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 5 11 0 0)
                          (moment 1998 5 11 1 0)
                          (moment 1998 5 11 2 0))))

(test-case "test-hourly-by-week-no-and-week-day"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:byweeknumber '(1)
                                #:byday '(monday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 29 0 0)
                          (moment 1997 12 29 1 0)
                          (moment 1997 12 29 2 0))))

(test-case "test-hourly-by-week-no-and-week-day-large"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:byweeknumber '(52)
                                #:byday '(sunday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 28 0 0)
                          (moment 1997 12 28 1 0)
                          (moment 1997 12 28 2 0))))

(test-case "test-hourly-by-week-no-and-week-day-last"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:byweeknumber '(-1)
                                #:byday '(sunday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 28 0 0)
                          (moment 1997 12 28 1 0)
                          (moment 1997 12 28 2 0))))

(test-case "test-hourly-by-week-no-and-week-day53"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:byweeknumber '(53)
                                #:byday '(monday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 12 28 0 0)
                          (moment 1998 12 28 1 0)
                          (moment 1998 12 28 2 0))))

(test-case "test-hourly-by-hour"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:byhour '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 0)
                          (moment 1997 9 3 6 0)
                          (moment 1997 9 3 18 0))))

(test-case "test-hourly-by-minute"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:byminute '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 6)
                          (moment 1997 9 2 9 18)
                          (moment 1997 9 2 10 6))))

(test-case "test-hourly-by-second"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0 6)
                          (moment 1997 9 2 9 0 18)
                          (moment 1997 9 2 10 0 6))))

(test-case "test-hourly-by-hour-and-minute"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:byhour '(6 18)
                                #:byminute '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 6)
                          (moment 1997 9 2 18 18)
                          (moment 1997 9 3 6 6))))

(test-case "test-hourly-by-hour-and-second"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:byhour '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 0 6)
                          (moment 1997 9 2 18 0 18)
                          (moment 1997 9 3 6 0 6))))

(test-case "test-hourly-by-minute-and-second"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:byminute '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 6 6)
                          (moment 1997 9 2 9 6 18)
                          (moment 1997 9 2 9 18 6))))

(test-case "test-hourly-by-hour-and-minute-and-second"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:byhour '(6 18)
                                #:byminute '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 6 6)
                          (moment 1997 9 2 18 6 18)
                          (moment 1997 9 2 18 18 6))))

(test-case "test-hourly-by-set-pos"
  (check-rrule-list (make-rrule #:freq 'hourly
                                #:count 3
                                #:byminute '(15 45)
                                #:bysecond '(15 45)
                                #:bysetpos '(3 -3))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 15 45)
                          (moment 1997 9 2 9 45 15)
                          (moment 1997 9 2 10 15 45))))

(test-case "test-minutely"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 2 9 1)
                          (moment 1997 9 2 9 2))))

(test-case "test-minutely-interval"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:interval 2)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 2 9 2)
                          (moment 1997 9 2 9 4))))

(test-case "test-minutely-interval-large"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:interval 1501)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 3 10 1)
                          (moment 1997 9 4 11 2))))

(test-case "test-minutely-by-month"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:bymonth '(1 3))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 0 0)
                          (moment 1998 1 1 0 1)
                          (moment 1998 1 1 0 2))))

(test-case "test-minutely-by-month-day"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:bymonthday '(1 3))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 3 0 0)
                          (moment 1997 9 3 0 1)
                          (moment 1997 9 3 0 2))))

(test-case "test-minutely-by-month-and-month-day"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:bymonth '(1 3)
                                #:bymonthday '(5 7))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 5 0 0)
                          (moment 1998 1 5 0 1)
                          (moment 1998 1 5 0 2))))

(test-case "test-minutely-by-week-day"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 2 9 1)
                          (moment 1997 9 2 9 2))))

(test-case "test-minutely-by-n-week-day"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:byday '((1 tuesday) (-1 thursday)))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 2 9 1)
                          (moment 1997 9 2 9 2))))

(test-case "test-minutely-by-month-and-week-day"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:bymonth '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 0 0)
                          (moment 1998 1 1 0 1)
                          (moment 1998 1 1 0 2))))

(test-case "test-minutely-by-month-and-n-week-day"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:bymonth '(1 3)
                                #:byday '((1 tuesday) (-1 thursday)))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 0 0)
                          (moment 1998 1 1 0 1)
                          (moment 1998 1 1 0 2))))

(test-case "test-minutely-by-month-day-and-week-day"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:bymonthday '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 0 0)
                          (moment 1998 1 1 0 1)
                          (moment 1998 1 1 0 2))))

(test-case "test-minutely-by-month-and-month-day-and-week-day"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:bymonth '(1 3)
                                #:bymonthday '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 0 0)
                          (moment 1998 1 1 0 1)
                          (moment 1998 1 1 0 2))))

(test-case "test-minutely-by-year-day"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 4
                                #:byyearday '(1 100 200 365))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 31 0 0)
                          (moment 1997 12 31 0 1)
                          (moment 1997 12 31 0 2)
                          (moment 1997 12 31 0 3))))

(test-case "test-minutely-by-year-day-neg"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 4
                                #:byyearday '(-365 -266 -166 -1))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 31 0 0)
                          (moment 1997 12 31 0 1)
                          (moment 1997 12 31 0 2)
                          (moment 1997 12 31 0 3))))

(test-case "test-minutely-by-month-and-year-day"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 4
                                #:bymonth '(4 7)
                                #:byyearday '(1 100 200 365))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 4 10 0 0)
                          (moment 1998 4 10 0 1)
                          (moment 1998 4 10 0 2)
                          (moment 1998 4 10 0 3))))

(test-case "test-minutely-by-month-and-year-day-neg"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 4
                                #:bymonth '(4 7)
                                #:byyearday '(-365 -266 -166 -1))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 4 10 0 0)
                          (moment 1998 4 10 0 1)
                          (moment 1998 4 10 0 2)
                          (moment 1998 4 10 0 3))))

(test-case "test-minutely-by-week-no"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:byweeknumber '(20))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 5 11 0 0)
                          (moment 1998 5 11 0 1)
                          (moment 1998 5 11 0 2))))

(test-case "test-minutely-by-week-no-and-week-day"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:byweeknumber '(1)
                                #:byday '(monday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 29 0 0)
                          (moment 1997 12 29 0 1)
                          (moment 1997 12 29 0 2))))

(test-case "test-minutely-by-week-no-and-week-day-large"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:byweeknumber '(52)
                                #:byday '(sunday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 28 0 0)
                          (moment 1997 12 28 0 1)
                          (moment 1997 12 28 0 2))))

(test-case "test-minutely-by-week-no-and-week-day-last"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:byweeknumber '(-1)
                                #:byday '(sunday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 28 0 0)
                          (moment 1997 12 28 0 1)
                          (moment 1997 12 28 0 2))))

(test-case "test-minutely-by-week-no-and-week-day53"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:byweeknumber '(53)
                                #:byday '(monday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 12 28 0 0)
                          (moment 1998 12 28 0 1)
                          (moment 1998 12 28 0 2))))

(test-case "test-minutely-by-hour"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:byhour '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 0)
                          (moment 1997 9 2 18 1)
                          (moment 1997 9 2 18 2))))

(test-case "test-minutely-by-minute"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:byminute '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 6)
                          (moment 1997 9 2 9 18)
                          (moment 1997 9 2 10 6))))

(test-case "test-minutely-by-second"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0 6)
                          (moment 1997 9 2 9 0 18)
                          (moment 1997 9 2 9 1 6))))

(test-case "test-minutely-by-hour-and-minute"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:byhour '(6 18)
                                #:byminute '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 6)
                          (moment 1997 9 2 18 18)
                          (moment 1997 9 3 6 6))))

(test-case "test-minutely-by-hour-and-second"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:byhour '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 0 6)
                          (moment 1997 9 2 18 0 18)
                          (moment 1997 9 2 18 1 6))))

(test-case "test-minutely-by-minute-and-second"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:byminute '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 6 6)
                          (moment 1997 9 2 9 6 18)
                          (moment 1997 9 2 9 18 6))))

(test-case "test-minutely-by-hour-and-minute-and-second"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:byhour '(6 18)
                                #:byminute '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 6 6)
                          (moment 1997 9 2 18 6 18)
                          (moment 1997 9 2 18 18 6))))

(test-case "test-minutely-by-set-pos"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 3
                                #:bysecond '(15 30 45)
                                #:bysetpos '(3 -3))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0 15)
                          (moment 1997 9 2 9 0 45)
                          (moment 1997 9 2 9 1 15))))

(test-case "test-secondly"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0 0)
                          (moment 1997 9 2 9 0 1)
                          (moment 1997 9 2 9 0 2))))

(test-case "test-secondly-interval"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:interval 2)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0 0)
                          (moment 1997 9 2 9 0 2)
                          (moment 1997 9 2 9 0 4))))

(test-case "test-secondly-interval-large"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:interval 90061)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0 0)
                          (moment 1997 9 3 10 1 1)
                          (moment 1997 9 4 11 2 2))))

(test-case "test-secondly-by-month"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:bymonth '(1 3))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 0 0 0)
                          (moment 1998 1 1 0 0 1)
                          (moment 1998 1 1 0 0 2))))

(test-case "test-secondly-by-month-day"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:bymonthday '(1 3))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 3 0 0 0)
                          (moment 1997 9 3 0 0 1)
                          (moment 1997 9 3 0 0 2))))

(test-case "test-secondly-by-month-and-month-day"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:bymonth '(1 3)
                                #:bymonthday '(5 7))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 5 0 0 0)
                          (moment 1998 1 5 0 0 1)
                          (moment 1998 1 5 0 0 2))))

(test-case "test-secondly-by-week-day"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0 0)
                          (moment 1997 9 2 9 0 1)
                          (moment 1997 9 2 9 0 2))))

(test-case "test-secondly-by-n-week-day"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:byday '((1 tuesday) (-1 thursday)))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0 0)
                          (moment 1997 9 2 9 0 1)
                          (moment 1997 9 2 9 0 2))))

(test-case "test-secondly-by-month-and-week-day"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:bymonth '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 0 0 0)
                          (moment 1998 1 1 0 0 1)
                          (moment 1998 1 1 0 0 2))))

(test-case "test-secondly-by-month-and-n-week-day"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:bymonth '(1 3)
                                #:byday '((1 tuesday) (-1 thursday)))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 0 0 0)
                          (moment 1998 1 1 0 0 1)
                          (moment 1998 1 1 0 0 2))))

(test-case "test-secondly-by-month-day-and-week-day"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:bymonthday '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 0 0 0)
                          (moment 1998 1 1 0 0 1)
                          (moment 1998 1 1 0 0 2))))

(test-case "test-secondly-by-month-and-month-day-and-week-day"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:bymonth '(1 3)
                                #:bymonthday '(1 3)
                                #:byday '(tuesday thursday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 1 0 0 0)
                          (moment 1998 1 1 0 0 1)
                          (moment 1998 1 1 0 0 2))))

(test-case "test-secondly-by-year-day"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 4
                                #:byyearday '(1 100 200 365))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 31 0 0 0)
                          (moment 1997 12 31 0 0 1)
                          (moment 1997 12 31 0 0 2)
                          (moment 1997 12 31 0 0 3))))

(test-case "test-secondly-by-year-day-neg"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 4
                                #:byyearday '(-365 -266 -166 -1))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 31 0 0 0)
                          (moment 1997 12 31 0 0 1)
                          (moment 1997 12 31 0 0 2)
                          (moment 1997 12 31 0 0 3))))

(test-case "test-secondly-by-month-and-year-day"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 4
                                #:bymonth '(4 7)
                                #:byyearday '(1 100 200 365))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 4 10 0 0 0)
                          (moment 1998 4 10 0 0 1)
                          (moment 1998 4 10 0 0 2)
                          (moment 1998 4 10 0 0 3))))

(test-case "test-secondly-by-month-and-year-day-neg"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 4
                                #:bymonth '(4 7)
                                #:byyearday '(-365 -266 -166 -1))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 4 10 0 0 0)
                          (moment 1998 4 10 0 0 1)
                          (moment 1998 4 10 0 0 2)
                          (moment 1998 4 10 0 0 3))))

(test-case "test-secondly-by-week-no"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:byweeknumber '(20))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 5 11 0 0 0)
                          (moment 1998 5 11 0 0 1)
                          (moment 1998 5 11 0 0 2))))

(test-case "test-secondly-by-week-no-and-week-day"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:byweeknumber '(1)
                                #:byday '(monday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 29 0 0 0)
                          (moment 1997 12 29 0 0 1)
                          (moment 1997 12 29 0 0 2))))

(test-case "test-secondly-by-week-no-and-week-day-large"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:byweeknumber '(52)
                                #:byday '(sunday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 28 0 0 0)
                          (moment 1997 12 28 0 0 1)
                          (moment 1997 12 28 0 0 2))))

(test-case "test-secondly-by-week-no-and-week-day-last"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:byweeknumber '(-1)
                                #:byday '(sunday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 12 28 0 0 0)
                          (moment 1997 12 28 0 0 1)
                          (moment 1997 12 28 0 0 2))))

(test-case "test-secondly-by-week-no-and-week-day53"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:byweeknumber '(53)
                                #:byday '(monday))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 12 28 0 0 0)
                          (moment 1998 12 28 0 0 1)
                          (moment 1998 12 28 0 0 2))))

(test-case "test-secondly-by-hour"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:byhour '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 0 0)
                          (moment 1997 9 2 18 0 1)
                          (moment 1997 9 2 18 0 2))))

(test-case "test-secondly-by-minute"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:byminute '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 6 0)
                          (moment 1997 9 2 9 6 1)
                          (moment 1997 9 2 9 6 2))))

(test-case "test-secondly-by-second"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0 6)
                          (moment 1997 9 2 9 0 18)
                          (moment 1997 9 2 9 1 6))))

(test-case "test-secondly-by-hour-and-minute"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:byhour '(6 18)
                                #:byminute '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 6 0)
                          (moment 1997 9 2 18 6 1)
                          (moment 1997 9 2 18 6 2))))

(test-case "test-secondly-by-hour-and-second"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:byhour '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 0 6)
                          (moment 1997 9 2 18 0 18)
                          (moment 1997 9 2 18 1 6))))

(test-case "test-secondly-by-minute-and-second"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:byminute '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 6 6)
                          (moment 1997 9 2 9 6 18)
                          (moment 1997 9 2 9 18 6))))

(test-case "test-secondly-by-hour-and-minute-and-second"
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:byhour '(6 18)
                                #:byminute '(6 18)
                                #:bysecond '(6 18))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 18 6 6)
                          (moment 1997 9 2 18 6 18)
                          (moment 1997 9 2 18 18 6))))

(test-case "test-secondly-by-hour-and-minute-and-second-bug"
  ;; This explores a bug found by Mathieu Bridon.
  (check-rrule-list (make-rrule #:freq 'secondly
                                #:count 3
                                #:bysecond '(0)
                                #:byminute '(1))
                    (moment 2010 3 22 12 1)
                    (list (moment 2010 3 22 12 1)
                          (moment 2010 3 22 13 1)
                          (moment 2010 3 22 14 1))))

(test-case "test-long-integers (from dateutil)"
  (check-rrule-list (make-rrule #:freq 'minutely
                                #:count 2
                                #:interval 2
                                #:bymonth '(2)
                                #:byday '(3)
                                #:byhour '(6)
                                #:byminute '(6)
                                #:bysecond '(6))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 2 5 6 6 6)
                          (moment 1998 2 12 6 6 6)))
  (check-rrule-list (make-rrule #:freq 'yearly
                                #:count 2
                                #:bymonthday '(5)
                                #:byweeknumber '(2))
                    (moment 1997 9 2 9 0)
                    (list (moment 1998 1 5 9 0)
                          (moment 2004 1 5 9 0))))


(test-case "test-hourly-bad-rrule"
  ;; When `byhour` is specified with `#:freq HOURLY` there are certain
  ;; combinations of `dtstart` and `byhour` which result in an rrule with no
  ;; valid values. See https://github.com/dateutil/dateutil/issues/4
  (check-exn exn:fail:contract?
             (thunk
              (in-rrule
               (make-rrule #:freq 'hourly
                           #:interval 4
                           #:byhour '(7 11 15 19))
               (moment 1997 9 2 9 0)))))

(test-case "test-minutely-bad-rrule"
  (check-exn exn:fail:contract?
             (thunk
              (in-rrule
               (make-rrule #:freq 'minutely
                           #:interval 12
                           #:byminute '(10 11 25 39 50))
               (moment 1997 9 2 9 0)))))

(test-case "test-secondly-bad-rrule"
  (check-exn exn:fail:contract?
             (thunk
              (in-rrule
               (make-rrule #:freq 'secondly
                           #:interval 10
                           #:bysecond '(2 15 37 42 59))
               (moment 1997 9 2 9 0)))))

(test-case "test-minutely-bad-combo-rrule"
  ;; Certain values of `interval` in `rrule` when combined with certain
  ;; values of `byhour` create rules which apply to no valid dates.
  (check-exn exn:fail:contract?
             (thunk
              (in-rrule
               (make-rrule #:freq 'minutely
                           #:interval 120
                           #:byhour '(10 12 14 16)
                           #:count 2)
               (moment 1997 9 2 9 0)))))

(test-case "test-secondly-bad-combo-rrule"
  (check-exn exn:fail:contract?
             (thunk
              (in-rrule
               (make-rrule #:freq 'secondly
                           #:interval 360
                           #:byminute '(10 28 49)
                           #:count 4)
               (moment 1997 9 2 9 0))))
  (check-exn exn:fail:contract?
             (thunk
              (in-rrule
               (make-rrule #:freq 'secondly
                           #:interval 43200
                           #:byhour '(2 10 18 23)
                           #:count 4)
               (moment 1997 9 2 9 0)))))

(test-case "test-bad-until-count-rrule"
  ;; See rfc-5545 3.3.10
  (check-exn exn:fail:contract?
             (thunk
              (make-rrule #:freq 'daily
                          #:count 3
                          #:until (moment 1997 9 4 9 0)))))

(test-case "test-until-not-matching"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:until (moment 1997 9 5 8 0))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 3 9 0)
                          (moment 1997 9 4 9 0))))

(test-case "test-until-matching"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:until (moment 1997 9 4 9 0))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 3 9 0)
                          (moment 1997 9 4 9 0))))

(test-case "test-until-single"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:until (moment 1997 9 2 9 0))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0))))

(test-case "test-until-empty"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:until (moment 1997 9 1 9 0))
                    (moment 1997 9 2 9 0)
                    (list)))

(test-case "test-until-with-date"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:until date(1997 9 5))
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 3 9 0)
                          (moment 1997 9 4 9 0))))

(test-case "test-wk-st-interval-mo"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:interval 2
                                #:byday '(tuesday sunday)
                                #:wkst 'monday)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 7 9 0)
                          (moment 1997 9 16 9 0))))

(test-case "test-wk-st-interval-su"
  (check-rrule-list (make-rrule #:freq 'weekly
                                #:count 3
                                #:interval 2
                                #:byday '(tuesday sunday)
                                #:wkst 'sunday)
                    (moment 1997 9 2 9 0)
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 14 9 0)
                          (moment 1997 9 16 9 0))))

(test-case "test-dt-start-is-date"
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3)
                    (date 1997 9 2)
                    (list (moment 1997 9 2 0 0)
                          (moment 1997 9 3 0 0)
                          (moment 1997 9 4 0 0))))

(test-case "test-dt-start-with-microseconds"
  (define (μs->ns μs) (* 1000 μs))
  (check-rrule-list (make-rrule #:freq 'daily
                                #:count 3)
                    (moment 1997 9 2 9 0 0 (μs->ns 500000))
                    (list (moment 1997 9 2 9 0)
                          (moment 1997 9 3 9 0)
                          (moment 1997 9 4 9 0))))

(test-case "test-get-item"
  (check-equal?
   (sequence-ref (in-rrule (make-rrule #:freq 'daily
                                       #:count 3)
                           (moment 1997 9 2 9 0))
                 0)
   (moment 1997 9 2 9 0)))

(test-case "test-get-item-neg"
  (check-equal?
   (last (rrule->list (make-rrule #:freq 'daily
                                  #:count 3)
                      (moment 1997 9 2 9 0)))
   (moment 1997 9 4 9 0)))

(test-case "test-get-item-slice"
  (check-equal?
   (stream-take
    (stream-tail (sequence->stream
                  (in-rrule (make-rrule #:freq 'daily)
                            (moment 1997 9 2 9 0)))
                 1)
    (- 2 1))
   (list (moment 1997 9 3 9 0))))

(test-case "test-get-item-slice-empty"
  (check-equal?
   (rrule->list (make-rrule #:freq 'daily
                            #:count 3)
                (moment 1997 9 2 9 0))
   (list (moment 1997 9 2 9 0)
         (moment 1997 9 3 9 0)
         (moment 1997 9 4 9 0))))

(test-case "test-get-item-slice-step"
  (check-equal?
   (take-right (rrule->list
                (make-rrule #:freq 'daily
                            #:count 3)
                (moment 1997 9 2 9 0))
               2)
   (list (moment 1997 9 4 9 0)
         (moment 1997 9 2 9 0))))

(test-case "test-count"
  (check-equal?
   (sequence-length (in-rrule
                     (make-rrule #:freq 'daily
                                 #:count 3)
                     (moment 1997 9 2 9 0)))
   3))

(test-case "test-count-zero"
  (check-equal?
   (sequence-length (in-rrule
                     (make-rrule #:freq 'yearly
                                 #:count 0)
                     (moment 1997 9 2 9 0)))
   0))

(test-case "test-contains"
  (check-equal?
   (sequence-count (λ (x) (equal? x (moment 1997 9 3 9 0)))
                   (in-rrule
                    (make-rrule #:freq 'daily
                                #:count 3)
                    (moment 1997 9 2 9 0)))
   1))

(test-case "test-before"
  (check-equal?
   (for/last ([m (in-rrule (make-rrule #:freq 'daily)
                           (moment 1997 9 2 9 0))]
              #:when (moment<? m (moment 1997 9 5 9 0)))
     m)
   (moment 1997 9 4 9 0)))

(test-case "test-before-inc"
  (check-equal?
   (for/last ([m (in-rrule (make-rrule #:freq 'daily)
                           (moment 1997 9 2 9 0))]
              #:when (moment<=? m (moment 1997 9 5 9 0)))
     m)
   (moment 1997 9 5 9 0)))

(test-case "test-after"
  (check-equal?
   (for/first ([m (in-rrule (make-rrule #:freq 'daily)
                            (moment 1997 9 2 9 0))]
               #:when (moment>? m (moment 1997 9 4 9 0)))
     m)
   (moment 1997 9 5 9 0)))

(test-case "test-after-inc"
  (check-equal?
   (for/first ([m (in-rrule (make-rrule #:freq 'daily)
                            (moment 1997 9 2 9 0))]
               #:when (moment>=? m (moment 1997 9 4 9 0)))
     m)
   (moment 1997 9 4 9 0)))

(test-case "test-x-after"
  (check-equal?
   (stream-take (sequence->stream
                 (sequence-filter
                  (λ (m) (moment>? m (moment 1997 9 8 9 0)))
                  (in-rrule (make-rrule #:freq 'daily)
                            (moment 1997 9 2 9 0))))
                12)
   (list (moment 1997 9 9 9 0)
         (moment 1997 9 10 9 0)
         (moment 1997 9 11 9 0)
         (moment 1997 9 12 9 0)
         (moment 1997 9 13 9 0)
         (moment 1997 9 14 9 0)
         (moment 1997 9 15 9 0)
         (moment 1997 9 16 9 0)
         (moment 1997 9 17 9 0)
         (moment 1997 9 18 9 0)
         (moment 1997 9 19 9 0)
         (moment 1997 9 20 9 0))))

(test-case "test-x-after-inc"
  (check-equal?
   (stream-take (sequence->stream
                 (sequence-filter
                  (λ (m) (moment>=? m (moment 1997 9 8 9 0)))
                  (in-rrule (make-rrule #:freq 'daily)
                            (moment 1997 9 2 9 0))))
                12)
   (list (moment 1997 9 8 9 0)
         (moment 1997 9 9 9 0)
         (moment 1997 9 10 9 0)
         (moment 1997 9 11 9 0)
         (moment 1997 9 12 9 0)
         (moment 1997 9 13 9 0)
         (moment 1997 9 14 9 0)
         (moment 1997 9 15 9 0)
         (moment 1997 9 16 9 0)
         (moment 1997 9 17 9 0)
         (moment 1997 9 18 9 0)
         (moment 1997 9 19 9 0))))

(test-case "test-between"
  (check-equal?
   (for/list ([m (in-rrule (make-rrule #:freq 'daily)
                           (moment 1997 9 2 9 0))]
              #:when (and (moment<? (moment 1997 9 2 9 0) m)
                          (moment<? m (moment 1997 9 6 9 0))))
     m)
   (list (moment 1997 9 3 9 0)
         (moment 1997 9 4 9 0)
         (moment 1997 9 5 9 0))))

(test-case "test-between-inc"
  (check-equal?
   (for/list ([m (in-rrule (make-rrule #:freq 'daily)
                           (moment 1997 9 2 9 0))]
              #:when (and (moment<=? (moment 1997 9 2 9 0) m)
                          (moment<=? m (moment 1997 9 6 9 0))))
     m)
   (list (moment 1997 9 2 9 0)
         (moment 1997 9 3 9 0)
         (moment 1997 9 4 9 0)
         (moment 1997 9 5 9 0)
         (moment 1997 9 6 9 0))))

(test-case "test-cache-pre"
  (check-equal?
   (rrule->list
    (make-rrule #:freq 'daily #:count 15)
    (moment 1997 9 2 9 0))
   (list (moment 1997 9 2 9 0)
         (moment 1997 9 3 9 0)
         (moment 1997 9 4 9 0)
         (moment 1997 9 5 9 0)
         (moment 1997 9 6 9 0)
         (moment 1997 9 7 9 0)
         (moment 1997 9 8 9 0)
         (moment 1997 9 9 9 0)
         (moment 1997 9 10 9 0)
         (moment 1997 9 11 9 0)
         (moment 1997 9 12 9 0)
         (moment 1997 9 13 9 0)
         (moment 1997 9 14 9 0)
         (moment 1997 9 15 9 0)
         (moment 1997 9 16 9 0))))

(test-case "test-str"
  (check-equal?
   (rrule->list
    (parse-rrule "FREQ=YEARLY;COUNT=3")
    (moment 1997 9 2 9))
   (list (moment 1997 9 2 9 0)
         (moment 1998 9 2 9 0)
         (moment 1999 9 2 9 0))))

(test-case "test-str-with-tzid"
  (check-equal?
   (rrule->list
    (parse-rrule "FREQ=YEARLY;COUNT=3")
    (moment 1997 9 2 9 #:tz "America/New_York"))
   (list (moment 1997 9 2 9 0 #:tz "America/New_York")
         (moment 1998 9 2 9 0 #:tz "America/New_York")
         (moment 1999 9 2 9 0 #:tz "America/New_York"))))

;; TODO: separate rrule/dtstart module for parsing?
;; (test-case "test-str-with-conflicting-tzid"
;;     # RFC 5545 Section 3.3.5 FORM #2: DATE WITH UTC TIME
;;     # https://tools.ietf.org/html/rfc5545#section-3.3.5
;;     # The "TZID" property parameter MUST NOT be applied to DATE-TIME
;;     with self.assertRaises(ValueError):
;;         parse-rrule("DTSTART;TZID=America/New_York:19970902T090000Z\n"+
;;                  "FREQ=YEARLY;COUNT=3\n")
;;         )

(test-case "test-str-type"
  (check-not-false (parse-rrule "FREQ=YEARLY;COUNT=3"))
  (check-false (parse-rrule "FREQ=FOO")))

(test-case "test-str-case"
  (check-equal?
   (rrule->list
    (parse-rrule "freq=yearly;count=3")
    (moment (1997 9 2 9)))
   (list (moment 1997 9 2 9 0)
         (moment 1998 9 2 9 0)
         (moment 1999 9 2 9 0))))

(test-case "test-str-spaces"
  (check-equal?
   (rrule->list
    (parse-rrule " FREQ=YEARLY;COUNT=3 ")
    (moment 1997 9 2 9))
   (list (moment 1997 9 2 9 0)
         (moment 1998 9 2 9 0)
         (moment 1999 9 2 9 0))))

(test-case "test-str-spaces-and-lines"
  (check-equal?
   (rrule->list
    (parse-rrule "\nFREQ=YEARLY;COUNT=3\n")
    (moment 1997 9 2 9))
   (list (moment 1997 9 2 9 0)
         (moment 1998 9 2 9 0)
         (moment 1999 9 2 9 0))))

(test-case "test-str-keywords"
  (check-equal?
   (rrule->list
    (parse-rrule (~a "FREQ=YEARLY;COUNT=3;INTERVAL=3;"
                     "BYMONTH=3;byday=TH;BYMONTHDAY=3;"
                     "BYHOUR=3;BYMINUTE=3;BYSECOND=3\n"))
    (moment 1997 9 2 9))
   (list (moment 2033 3 3 3 3 3)
         (moment 2039 3 3 3 3 3)
         (moment 2072 3 3 3 3 3))))

(test-case "test-str-n-week-day"
  (check-equal?
   (rrule->list
    (parse-rrule "FREQ=YEARLY;COUNT=3;BYDAY=1TU-1TH\n")
    (moment 1997 9 2 9))
   (list (moment 1997 12 25 9 0)
         (moment 1998 1 6 9 0)
         (moment 1998 12 31 9 0))))

(test-case "test-str-until"
  (check-equal?
   (rrule->list
    ;; The value of the UNTIL rule part MUST have the same value type as the
    ;; "DTSTART" property.  Furthermore, if the "DTSTART" property is specified
    ;; as a date with local time, then the UNTIL rule part MUST also be
    ;; specified as a date with local time.
    (parse-rrule "FREQ=YEARLY;UNTIL=19990101T000000;BYDAY=1TU-1TH\n")
    (moment 1997 9 2 9))
   (list (moment 1997 12 25 9 0)
         (moment 1998 1 6 9 0)
         (moment 1998 12 31 9 0))))

(test-case "test-str-value-date"
  (check-equal?
   (rrule->list
    (parse-rrule "FREQ=YEARLY;COUNT=2")
    (date 1997 9 2))
   (list (moment 1997 9 2 0 0 0)
         (moment 1998 9 2 0 0 0))))

(test-case "test-str-invalid-until"
  (check-false
   (parse-rrule "FREQ=YEARLY;UNTIL=TheCowsComeHome;BYDAY=1TU-1TH\n")))

(test-case "test-str-until-must-be-utc"
  (check-exn
   exn:fail:contract?
   (thunk
    (rrule->list
    ;; If the "DTSTART" property is specified as a date with UTC time or a date
    ;; with local time and time zone reference, then the UNTIL rule part MUST be
    ;; specified as a date with UTC time.
    (parse-rrule "FREQ=YEARLY;UNTIL=19990101T000000Z;BYDAY=1TU,-1TH")
    (moment 1997 1 1 0 0 0 #:tz "America/New_York")))))

(test-case "test-str-until-with-tz"
  (check-equal?
   (rrule->list
    (parse-rrule "FREQ=YEARLY;UNTIL=19990101T000000Z")
    (moment 1997 1 1 0 0 0 #:tz "America/New_York"))
   (list
    (moment 1997 1 1 0 0 0 #:tz "America/New_York")
    (moment 1998 1 1 0 0 0 #:tz "America/New_York"))))

(test-case "test-str-empty-by-day"
  (check-false
   (parse-rrule
    (~a "FREQ=WEEKLY;"
        "BYDAY=;"         ;; This part is invalid
        "WKST=SU"))))

(test-case "test-str-invalid-by-day"
  (check-false
   (parse-rrule
    (~a
     "FREQ=WEEKLY;"
     "BYDAY=-1OK;"         ;; This part is invalid
     "WKST=SU"))))

(test-case "test-bad-by-set-pos"
  (check-exn
   exn:fail:contract?
   (thunk
    (make-rrule #:freq 'monthly
                #:count 1
                #:bysetpos '(0)))))

(test-case "test-bad-by-set-pos-many"
  (check-exn
   exn:fail:contract?
   (thunk (make-rrule #:freq 'monthly
                      #:count 1
                      #:bysetpos '(-1 0 1)))))

(test-case "test-to-str-yearly"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly #:count 3)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-interval"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly #:count 3 #:interval 2)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-month"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:bymonth '(1 3))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-month-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:bymonthday '(1 3))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-month-and-month-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:bymonth '(1 3)
               #:bymonthday '(5 7))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-n-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:byday '((1 tuesday) (-1 thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-n-week-day-large"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:byday '((3 tuesday) (-3 thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-month-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:bymonth '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-month-and-n-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:bymonth '(1 3)
               #:byday '((1 tuesday) (-1 thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-month-and-n-week-day-large"
  ;; This is interesting because the TH(-3) ends up before the TU(3).
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:bymonth '(1 3)
               #:byday '((3 tuesday) (-3 thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-month-day-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:bymonthday '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-month-and-month-day-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:bymonth '(1 3)
               #:bymonthday '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-year-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 4
               #:byyearday '(1 100 200 365))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-year-day-neg"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 4
               #:byyearday '(-365 -266 -166 -1))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-month-and-year-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 4
               #:bymonth '(4 7)
               #:byyearday '(1 100 200 365))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-month-and-year-day-neg"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 4
               #:bymonth '(4 7)
               #:byyearday '(-365 -266 -166 -1))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-week-no"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:byweeknumber '(20))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-week-no-and-week-day"
  ;; That's a nice one. The first days of week number one may be in the last
  ;; year.
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:byweeknumber '(1)
               #:byday '(monday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-week-no-and-week-day-large"
  ;; Another nice test. The last days of week number 52/53 may be in the next
  ;; year.
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:byweeknumber '(52)
               #:byday '(sunday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-week-no-and-week-day-last"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:byweeknumber '(-1)
               #:byday '(sunday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-easter"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:byeaster 0)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-easter-pos"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:byeaster 1)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-easter-neg"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:byeaster -1)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-week-no-and-week-day53"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:byweeknumber '(53)
               #:byday '(monday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-hour"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:byhour '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-minute"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:byminute '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-hour-and-minute"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:byhour '(6 18)
               #:byminute '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-hour-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:byhour '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-minute-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:byminute '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-hour-and-minute-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:byhour '(6 18)
               #:byminute '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-yearly-by-set-pos"
  (check-roundtrip-through-string
   (make-rrule #:freq 'yearly
               #:count 3
               #:bymonthday 15
               #:byhour '(6 18)
               #:bysetpos (3 -3))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-interval"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:interval 2)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-interval-large"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:interval 18)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-month"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:bymonth '(1 3))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-month-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:bymonthday '(1 3))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-month-and-month-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:bymonth '(1 3)
               #:bymonthday '(5 7))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0))

  ;; Third Monday of the month
  ;; Ben: I'm not sure why this is here, but I'll keep it for consistency with
  ;; the Python tests.
  (check-equal?
   (for/list ([m (in-rrule (make-rrule #:freq 'monthly
                                       #:byday '((3 monday))
                                       (moment 1997 9 1)))]
              #:when (and (moment<? (moment 1997 9 1) m)
                          (moment<? m (moment 1997 12 1))))
     m)
   (list (moment 1997 9 15 0 0)
         (moment 1997 10 20 0 0)
         (moment 1997 11 17 0 0))))

(test-case "test-to-str-monthly-by-n-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:byday '((1 tuesday) (-1 thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-n-week-day-large"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:byday '((3 tuesday) (-3 thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-month-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:bymonth '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-month-and-n-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:bymonth '(1 3)
               #:byday '((1 tuesday) (-1 thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-month-and-n-week-day-large"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:bymonth '(1 3)
               #:byday '((3 tuesday) (-3 thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-month-day-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:bymonthday '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-month-and-month-day-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:bymonth '(1 3)
               #:bymonthday '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-year-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 4
               #:byyearday '(1 100 200 365))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-year-day-neg"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 4
               #:byyearday '(-365 -266 -166 -1))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-month-and-year-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 4
               #:bymonth '(4 7)
               #:byyearday '(1 100 200 365))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-month-and-year-day-neg"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 4
               #:bymonth '(4 7)
               #:byyearday '(-365 -266 -166 -1))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-week-no"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:byweeknumber '(20))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-week-no-and-week-day"
  ;; That's a nice one. The first days of week number one may be in the last
  ;; year.
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:byweeknumber '(1)
               #:byday '(monday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-week-no-and-week-day-large"
  ;; Another nice test. The last days of week number 52/53 may be in the next
  ;; year.
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:byweeknumber '(52)
               #:byday '(sunday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-week-no-and-week-day-last"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:byweeknumber '(-1)
               #:byday '(sunday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-week-no-and-week-day53"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:byweeknumber '(53)
               #:byday '(monday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-easter"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:byeaster 0)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-easter-pos"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:byeaster 1)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-easter-neg"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:byeaster -1)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-hour"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:byhour '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-minute"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:byminute '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-hour-and-minute"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:byhour '(6 18)
               #:byminute '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-hour-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:byhour '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-minute-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:byminute '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-hour-and-minute-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:byhour '(6 18)
               #:byminute '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-monthly-by-set-pos"
  (check-roundtrip-through-string
   (make-rrule #:freq 'monthly
               #:count 3
               #:bymonthday '(13 17)
               #:byhour '(6 18)
               #:bysetpos (3 -3))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-interval"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:interval 2)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-interval-large"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:interval 20)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-month"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:bymonth '(1 3))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-month-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:bymonthday '(1 3))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-month-and-month-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:bymonth '(1 3)
               #:bymonthday '(5 7))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-n-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:byday '((1 tuesday) (-1 thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-month-and-week-day"
  ;; This test is interesting because it crosses the year boundary in a weekly
  ;; period to find day '1' as a valid recurrence.
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:bymonth '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-month-and-n-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:bymonth '(1 3)
               #:byday '((1 tuesday) (-1 thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-month-day-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:bymonthday '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-month-and-month-day-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:bymonth '(1 3)
               #:bymonthday '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-year-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 4
               #:byyearday '(1 100 200 365))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-year-day-neg"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 4
               #:byyearday '(-365 -266 -166 -1))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-month-and-year-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 4
               #:bymonth '(1 7)
               #:byyearday '(1 100 200 365))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-month-and-year-day-neg"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 4
               #:bymonth '(1 7)
               #:byyearday '(-365 -266 -166 -1))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-week-no"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:byweeknumber '(20))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-week-no-and-week-day"
  ;; That's a nice one. The first days of week number one may be in the last
  ;; year.
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:byweeknumber '(1)
               #:byday '(monday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-week-no-and-week-day-large"
  ;; Another nice test. The last days of week number 52/53 may be in the next
  ;; year.
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:byweeknumber '(52)
               #:byday '(sunday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-week-no-and-week-day-last"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:byweeknumber '(-1)
               #:byday '(sunday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-week-no-and-week-day53"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:byweeknumber '(53)
               #:byday '(monday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-easter"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:byeaster 0)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-easter-pos"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:byeaster 1)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-easter-neg"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:byeaster -1)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-hour"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:byhour '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-minute"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:byminute '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-hour-and-minute"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:byhour '(6 18)
               #:byminute '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-hour-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:byhour '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-minute-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:byminute '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-hour-and-minute-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:byhour '(6 18)
               #:byminute '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-weekly-by-set-pos"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:byday '((tuesday) (thursday))
               #:byhour '(6 18)
               #:bysetpos (3 -3))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-interval"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:interval 2)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-interval-large"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:interval 92)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-month"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:bymonth '(1 3))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-month-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:bymonthday '(1 3))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-month-and-month-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:bymonth '(1 3)
               #:bymonthday '(5 7))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-n-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:byday '((1 tuesday) (-1 thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-month-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:bymonth '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-month-and-n-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:bymonth '(1 3)
               #:byday '((1 tuesday) (-1 thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-month-day-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:bymonthday '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-month-and-month-day-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:bymonth '(1 3)
               #:bymonthday '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-year-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 4
               #:byyearday '(1 100 200 365))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-year-day-neg"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 4
               #:byyearday '(-365 -266 -166 -1))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-month-and-year-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 4
               #:bymonth '(1 7)
               #:byyearday '(1 100 200 365))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-month-and-year-day-neg"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 4
               #:bymonth '(1 7)
               #:byyearday '(-365 -266 -166 -1))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-week-no"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:byweeknumber '(20))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-week-no-and-week-day"
  ;; That's a nice one. The first days of week number one may be in the last
  ;; year.
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:byweeknumber '(1)
               #:byday '(monday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-week-no-and-week-day-large"
  ;; Another nice test. The last days of week number 52/53 may be in the next
  ;; year.
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:byweeknumber '(52)
               #:byday '(sunday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-week-no-and-week-day-last"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:byweeknumber '(-1)
               #:byday '(sunday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-week-no-and-week-day53"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:byweeknumber '(53)
               #:byday '(monday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-easter"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:byeaster 0)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-easter-pos"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:byeaster 1)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-easter-neg"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:byeaster -1)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-hour"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:byhour '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-minute"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:byminute '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-hour-and-minute"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:byhour '(6 18)
               #:byminute '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-hour-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:byhour '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-minute-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:byminute '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-hour-and-minute-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:byhour '(6 18)
               #:byminute '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-daily-by-set-pos"
  (check-roundtrip-through-string
   (make-rrule #:freq 'daily
               #:count 3
               #:byhour '(6 18)
               #:byminute '(15 45)
               #:bysetpos (3 -3))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-interval"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:interval 2)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-interval-large"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:interval 769)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-month"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:bymonth '(1 3))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-month-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:bymonthday '(1 3))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-month-and-month-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:bymonth '(1 3)
               #:bymonthday '(5 7))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-n-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:byday '((1 tuesday) (-1 thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-month-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:bymonth '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-month-and-n-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:bymonth '(1 3)
               #:byday '((1 tuesday) (-1 thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-month-day-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:bymonthday '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-month-and-month-day-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:bymonth '(1 3)
               #:bymonthday '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-year-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 4
               #:byyearday '(1 100 200 365))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-year-day-neg"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 4
               #:byyearday '(-365 -266 -166 -1))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-month-and-year-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 4
               #:bymonth '(4 7)
               #:byyearday '(1 100 200 365))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-month-and-year-day-neg"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 4
               #:bymonth '(4 7)
               #:byyearday '(-365 -266 -166 -1))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-week-no"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:byweeknumber '(20))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-week-no-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:byweeknumber '(1)
               #:byday '(monday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-week-no-and-week-day-large"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:byweeknumber '(52)
               #:byday '(sunday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-week-no-and-week-day-last"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:byweeknumber '(-1)
               #:byday '(sunday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-week-no-and-week-day53"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:byweeknumber '(53)
               #:byday '(monday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-easter"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:byeaster 0)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-easter-pos"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:byeaster 1)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-easter-neg"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:byeaster -1)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-hour"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:byhour '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-minute"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:byminute '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-hour-and-minute"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:byhour '(6 18)
               #:byminute '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-hour-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:byhour '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-minute-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:byminute '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-hour-and-minute-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:byhour '(6 18)
               #:byminute '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-hourly-by-set-pos"
  (check-roundtrip-through-string
   (make-rrule #:freq 'hourly
               #:count 3
               #:byminute '(15 45)
               #:bysecond '(15 45)
               #:bysetpos (3 -3))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-interval"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:interval 2)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-interval-large"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:interval 1501)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-month"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:bymonth '(1 3))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-month-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:bymonthday '(1 3))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-month-and-month-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:bymonth '(1 3)
               #:bymonthday '(5 7))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-n-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:byday '((1 tuesday) (-1 thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-month-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:bymonth '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-month-and-n-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:bymonth '(1 3)
               #:byday '((1 tuesday) (-1 thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-month-day-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:bymonthday '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-month-and-month-day-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:bymonth '(1 3)
               #:bymonthday '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-year-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 4
               #:byyearday '(1 100 200 365))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-year-day-neg"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 4
               #:byyearday '(-365 -266 -166 -1))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-month-and-year-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 4
               #:bymonth '(4 7)
               #:byyearday '(1 100 200 365))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-month-and-year-day-neg"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 4
               #:bymonth '(4 7)
               #:byyearday '(-365 -266 -166 -1))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-week-no"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:byweeknumber '(20))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-week-no-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:byweeknumber '(1)
               #:byday '(monday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-week-no-and-week-day-large"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:byweeknumber '(52)
               #:byday '(sunday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-week-no-and-week-day-last"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:byweeknumber '(-1)
               #:byday '(sunday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-week-no-and-week-day53"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:byweeknumber '(53)
               #:byday '(monday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-easter"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:byeaster 0)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-easter-pos"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:byeaster 1)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-easter-neg"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:byeaster -1)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-hour"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:byhour '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-minute"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:byminute '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-hour-and-minute"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:byhour '(6 18)
               #:byminute '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-hour-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:byhour '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-minute-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:byminute '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-hour-and-minute-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:byhour '(6 18)
               #:byminute '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-minutely-by-set-pos"
  (check-roundtrip-through-string
   (make-rrule #:freq 'minutely
               #:count 3
               #:bysecond '(15 30 45)
               #:bysetpos (3 -3))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-interval"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:interval 2)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-interval-large"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:interval 90061)
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-month"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:bymonth '(1 3))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-month-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:bymonthday '(1 3))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-month-and-month-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:bymonth '(1 3)
               #:bymonthday '(5 7))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-n-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:byday '((1 tuesday) (-1 thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-month-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:bymonth '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-month-and-n-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:bymonth '(1 3)
               #:byday '((1 tuesday) (-1 thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-month-day-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:bymonthday '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-month-and-month-day-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:bymonth '(1 3)
               #:bymonthday '(1 3)
               #:byday '((tuesday) (thursday)))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-year-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 4
               #:byyearday '(1 100 200 365))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-year-day-neg"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 4
               #:byyearday '(-365 -266 -166 -1))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-month-and-year-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 4
               #:bymonth '(4 7)
               #:byyearday '(1 100 200 365))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-month-and-year-day-neg"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 4
               #:bymonth '(4 7)
               #:byyearday '(-365 -266 -166 -1))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-week-no"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:byweeknumber '(20))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-week-no-and-week-day"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:byweeknumber '(1)
               #:byday '(monday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-week-no-and-week-day-large"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:byweeknumber '(52)
               #:byday '(sunday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-week-no-and-week-day-last"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:byweeknumber '(-1)
               #:byday '(sunday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-week-no-and-week-day53"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:byweeknumber '(53)
               #:byday '(monday))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-hour"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:byhour '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-minute"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:byminute '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-hour-and-minute"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:byhour '(6 18)
               #:byminute '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-hour-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:byhour '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-minute-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:byminute '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-hour-and-minute-and-second"
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:byhour '(6 18)
               #:byminute '(6 18)
               #:bysecond '(6 18))
   (moment 1997 9 2 9 0)))

(test-case "test-to-str-secondly-by-hour-and-minute-and-second-bug"
  ;; This explores a bug found by Mathieu Bridon.
  (check-roundtrip-through-string
   (make-rrule #:freq 'secondly
               #:count 3
               #:bysecond '(0)
               #:byminute '(1))
   (moment 2010 3 22 12 1)))

(test-case "test-to-str-with-wk-st"
  (check-roundtrip-through-string
   (make-rrule #:freq 'weekly
               #:count 3
               #:wkst 'sunday)
   (moment 1997 9 2 9 0)))
