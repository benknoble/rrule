#lang racket/base

(require gregor
         racket/contract
         racket/sequence)

(provide
 (contract-out
  [valid-rrule? (-> any/c any/c)]
  [valid-rrule/dtstart? (-> any/c dtstart/c any/c)]
  [make-rrule (->* (#:freq freq/c)
                   (#:until enddate/c
                    #:count exact-positive-integer?
                    #:interval exact-positive-integer?
                    #:bysecond byseclist/c
                    #:byminute byminlist/c
                    #:byhour byhrlist/c
                    #:byday bywdaylist/c
                    #:bymonthday bymodaylist/c
                    #:byyearday byyrdaylist/c
                    #:byweeknumber bywknolist/c
                    #:bymonth bymolist/c
                    #:bysetpos bysplist/c
                    #:wkst weekday/c)
                   (or/c #f valid-rrule?))]
  [repeats-forever? (-> valid-rrule? any/c)]
  [parse-rrule (-> string? (or/c #f valid-rrule?))]
  [rrule->string (-> valid-rrule? string?)]
  [in-rrule (->i ([r valid-rrule?] [d dtstart/c])
                 #:pre (r d) (valid-rrule/dtstart? r d)
                 [result (sequence/c moment?)])]
  [rrule->list (->i ([r (and/c valid-rrule? (not/c repeats-forever?))] [d dtstart/c])
                    #:pre (r d) (valid-rrule/dtstart? r d)
                    [result (listof moment?)])]))

(require racket/match)

(define freq/c (or/c 'secondly 'minutely 'hourly 'daily 'weekly 'monthly 'yearly))
(define seconds/c (integer-in 0 60))
(define byseclist/c (listof seconds/c))
(define minutes/c (integer-in 0 59))
(define byminlist/c (listof minutes/c))
(define hour/c (integer-in 0 23))
(define byhrlist/c (listof hour/c))
(define ordwk/c (or/c (integer-in 1 53)
                      (integer-in -53 -1)))
(define weekday/c (or/c 'monday 'tuesday 'wednesday 'thursday 'friday 'saturday 'sunday))
(define weekdaynum/c (or/c weekday/c
                           (list/c ordwk/c weekday/c)))
(define bywdaylist/c (listof weekdaynum/c))
(define ordmoday/c (or/c (integer-in 1 31)
                         (integer-in -31 -1)))
(define monthdaynum/c (list/c ordmoday/c))
(define bymodaylist/c (listof monthdaynum/c))
(define ordyrday/c (or/c (integer-in 1 366)
                         (integer-in -366 -1)))
(define yeardaynum/c ordyrday/c)
(define byyrdaylist/c (listof yeardaynum/c))
(define weeknum/c ordwk/c)
(define bywknolist/c (listof weeknum/c))
(define monthnum/c (integer-in 1 12))
(define bymolist/c (listof monthnum/c))
(define setposday/c yeardaynum/c)
(define bysplist/c (listof setposday/c))
(define enddate/c (or/c date? datetime? moment?))
(define dtstart/c enddate/c)

(define default-interval 1)
(define default-wkst 'monday)

(define (make-rrule
         #:freq freq
         #:until [until #f]
         #:count [count #f]
         #:interval [interval default-interval]
         #:bysecond [bysecond #f]
         #:byminute [byminute #f]
         #:byhour [byhour #f]
         #:byday [byday #f]
         #:bymonthday [bymonthday #f]
         #:byyearday [byyearday #f]
         #:byweeknumber [byweeknumber #f]
         #:bymonth [bymonth #f]
         #:bysetpos [bysetpos #f]
         #:wkst [wkst default-wkst])
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
   byweeknumber
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
                 (rrule-byweeknumber r)
                 (bywdaylist-contains-numeric-value? (rrule-byday r))))
       (not (and (eq? (rrule-freq r) 'weekly)
                 (rrule-bymonthday r)))
       (not (and (member (rrule-freq r) '(daily weekly monthly))
                 (rrule-byyearday r)))
       (not (and (not (eq? (rrule-freq r) 'yearly))
                 (rrule-byweeknumber r)))
       (not (and (rrule-bysetpos r)
                 (not (rrule-bysecond r))
                 (not (rrule-byminute r))
                 (not (rrule-byhour r))
                 (not (rrule-byday r))
                 (not (rrule-bymonthday r))
                 (not (rrule-byyearday r))
                 (not (rrule-byweeknumber r))
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

(define (repeats-forever? r)
  (and (not (rrule-until r))
       (not (rrule-count r))))

(define (rrule->string r)
  (error 'todo))

(define (bywdaylist-contains-numeric-value? w)
  (andmap (match-lambda
            [(list _weekday) #f]
            [(list _ordwk _weekday) #t]
            [(list _sign _ordwk _weekday) #t])
          w))

(define (parse-rrule str)
  (error 'todo))

(define (in-rrule rr dtstart)
  (error 'todo))

(define (rrule->list rr dtstart)
  (sequence->list (in-rrule rr dtstart)))

;; TODO: exdate
