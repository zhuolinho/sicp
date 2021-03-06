(load "test.scm")

(define (simpson f a b n)
    (define h (/ (- b a) n))
    (define (y k)
        (f (+ a (* k h)))
    )
    (define (term k)
        (define factor
            (cond ((= k 0) 1)
                ((= k n) 1)
                ((even? k) 2)
                (else 4)
            )
        )
        (* factor (y k))
    )
    (define (inc x) (+ x 1))
    (* (/ h 3) (sum term (exact->inexact 0) inc n))
)

(define (sum term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a)))
        )
    )
    (iter a 0)
)
