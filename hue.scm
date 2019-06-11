(load "scheme/fixed-point.scm")

(define dx 0.00001)

(define (double p)
    (lambda (x) (p (p x)))
)

(define (inc x)
    (+ x 1)
)

(define (compose f g)
    (lambda (x) (f (g x)))
)

(define (repeated fn n)
    (lambda (x)
        (define (iter i result)
            (if (= i n)
                result
                (iter (+ i 1) (fn result))
            )
        )
        (iter 0 x)
    )
)

(define (smooth fn)
    (lambda (x) (/ (+ (fn (- x dx)) (fn x) (fn (+ x dx))) 3))
)

(define (smooth-n-times fn n)
    ((repeated smooth n) fn)
)

(define (hue x n)
    (fixed-point (average-damp-n-times (lambda (y) (/ x (cube y n))) (lg n)) 1.0)
)

(define (cube x n)
    (define (iter i result)
        (if (= n i)
            result
            (iter (+ i 1) (* x result))
        )
    )
    (iter 1 1)
)

(define (average-damp-n-times fn n)
    ((repeated average-damp n) fn)
)

(define (lg n)
    (cond ((> (/ n 2) 1) (+ (lg (/ n 2)) 1))
        ((< (/ n 2) 1) 0)
        (else 1)
    )
)
