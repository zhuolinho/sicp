(define (fib n)
    (display "a")
    (cond 
        ((= n 0) 0)
        ((= n 1) 1)
        (else (+
            (fib (- n 1))
            (fib (- n 2))))))

(define (lookup keys table)
    (if (list? keys)
        (if (null? keys)
            (cdr table)
            (let ((subtable (assoc (car keys) (cdr table))))
                (if subtable
                    (lookup (cdr keys) subtable)
                    false)))
        (lookup (list keys) table)))

(define (insert! keys value table)
    (if (list? keys)
        (if (null? keys)
            (set-cdr! table value)
            (let ((subtable (assoc (car keys) (cdr table))))
                (if subtable
                    (insert! (cdr keys) value subtable)
                    (let ((new-table (list (car keys))))
                        (set-cdr! table
                            (cons new-table (cdr table)))
                        (insert! (cdr keys) value new-table)))))
        (insert! (list keys) value table))
    'ok)

(define (make-table)
    (list '*table*))

(define (memoize f)
    (let ((table (make-table)))
        (lambda (x)
            (let ((pcr (lookup x table)))
                (or pcr
                    (let ((result (f x)))
                        (insert! x result table)
                        result))))))

(define memo-fib
    (memoize (lambda (n)
        (display "a")
        (cond 
            ((= n 0) 0)
            ((= n 1) 1)
            (else (+
                (memo-fib (- n 1))
                (memo-fib (- n 2))))))))

(memo-fib 2)