#lang racket/base

(require gregor
         racket/contract
         racket/match)

(define freq/c (or/c 'secondly 'minutely 'hourly 'daily 'weekly 'monthly 'yearly))
(define seconds/c (integer-in 0 60))
(define byseclist/c (listof seconds/c))
(define minutes/c (integer-in 0 59))
(define byminlist/c (listof minutes/c))
(define hour/c (integer-in 0 23))
(define byhrlist/c (listof hour/c))
(define sign/c (or/c '+ '-))
(define ordwk/c (integer-in 1 53))
(define weekday/c (or/c 'monday 'tuesday 'wednesday 'thursday 'friday 'saturday 'sunday))
(define weekdaynum/c (or/c (list/c weekday/c)
                           (list/c ordwk/c weekday/c)
                           (list/c sign/c ordwk/c weekday/c)))
(define bywdaylist/c (listof weekdaynum/c))
(define ordmoday/c (integer-in 1 31))
(define monthdaynum/c (or/c (list/c ordmoday/c)
                            (list/c sign/c ordmoday/c)))
(define bymodaylist/c (listof monthdaynum/c))
(define ordyrday/c (integer-in 1 366))
(define yeardaynum/c (or/c (list/c ordyrday/c)
                           (list/c sign/c ordyrday/c)))
(define byyrdaylist/c (listof yeardaynum/c))
(define weeknum/c (or/c (list/c ordwk/c)
                        (list/c sign/c ordwk/c)))
(define bywknolist/c (listof weeknum/c))
(define monthnum/c (integer-in 1 12))
(define bymolist/c (listof monthnum/c))
(define setposday/c yeardaynum/c)
(define bysplist/c (listof setposday/c))
(define enddate/c (or/c date? datetime? moment?))
(define dtstart/c enddate/c)

(define default-interval 1)
(define default-wkst 'monday)

(define (make-rrule #|…|#)
  (error 'todo))

(struct rrule
  [freq
   until #| OR |# count
   interval
   bysecond
   byminute
   byhour
   byday
   bymonthday
   byyearday
   byweekno
   bymonth
   bysetpos
   wkst])

(define (valid-rrule? r)
  (and (rrule? r)
       (not (and (rrule-until r)
                 (rrule-count r)))
       (not (and (member (rrule-freq r) '(monthly yearly))
                 (bywdaylist-contains-numeric-value? (rrule-byday r))))
       (not (and (eq? (rrule-freq r) 'yearly)
                 (rrule-byweekno r)
                 (bywdaylist-contains-numeric-value? (rrule-byday r))))
       (not (and (eq? (rrule-freq r) 'weekly)
                 (rrule-bymonthday r)))
       (not (and (member (rrule-freq r) '(daily weekly monthly))
                 (rrule-byyearday r)))
       (not (and (not (eq? (rrule-freq r) 'yearly))
                 (rrule-byweekno r)))
       (not (and (rrule-bysetpos r)
                 (not (rrule-bysecond r))
                 (not (rrule-byminute r))
                 (not (rrule-byhour r))
                 (not (rrule-byday r))
                 (not (rrule-bymonthday r))
                 (not (rrule-byyearday r))
                 (not (rrule-byweekno r))
                 (not (rrule-bymonth r))))))

(define (valid-rrule/dtstart? r d)
  (and (valid-rrule? r)
       (let ([until (rrule-until r)])
         (or (not until)
             (and (date? d) (date? until))
             (and (datetime? d) (datetime? until))
             (and (moment? d) (moment? until)
                  (zero? (->utc-offset until)))))
       (not (and (date? d)
                 (or (rrule-bysecond r)
                     (rrule-byminute r)
                     (rrule-byhour r))))))

(define (repeats-foreve? r)
  (and (not (rrule-until r))
       (not (rrule-count r))))

(define (rrule->string r)
  (error 'todo))

(define (rrule+dstart->string r d)
  (error 'todo))

(define (bywdaylist-contains-numeric-value? w)
  (andmap (match-lambda
            [(list _weekday) #f]
            [(list _ordwk _weekday) #t]
            [(list _sign _ordwk _weekday) #t])
          w))
