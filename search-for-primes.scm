(load "continue-primes.scm")
(load "next.scm")
(load "expmod.scm")
; (define (search-for-primes n)
;     (let ((start-time (real-time-clock)))
;         (continue-primes n 3)
;         (- (real-time-clock) start-time)))

(define (prime? n)
    (fast-prime? n 10))

(define (timed-prime-test n)
    (display n)
    (newline)
    (start-prime-test n (real-time-clock)))

(define (start-prime-test n start-time)
    (continue-primes n 3)
        (report-prime (- (real-time-clock) start-time)))

(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))
