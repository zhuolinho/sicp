(load "prime.scm")

(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (real-time-clock)))

(define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (real-time-clock) start-time))))

(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))
