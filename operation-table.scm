(define (make-table)
    (let ((local-table (list '*table)))

        (define (lookup key-1 key-2)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (cdr record)
                            false
                        )
                    )
                    false
                )
            )
        )
        
        (define (insert! key-1 key-2 value)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (set-cdr! record value)
                            (set-cdr! 
                                subtable
                                (cons (cons key-2 value) (cdr subtable))
                            )
                        )
                    )
                    (set-cdr! 
                        local-table
                        (cons 
                            (list key-1 (cons key-2 value))
                            (cdr local-table)
                        )
                    )
                )
            )
            'ok
        )

        (define (dispatch m)
            (cond 
                ((eq? m 'lookup-proc) lookup)
                ((eq? m 'insert-proc!) insert!)
                (else (error "Unknown operation -- TABLE" m))
            )
        )

        dispatch
    )
)

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (deriv exp var)
    (cond 
        ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else
            ((get 'deriv (operator exp))
                (operands exp)
                var
            )
        )
    )
)

(define (operator exp)
    (car exp)
)

(define (operands exp)
    (cdr exp)
)

(define (variable? x)
    (symbol? x)
)

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))
)

(define (=number? exp num)
    (and (number? exp) (= exp num))
)

(define (install-sum-package)
    (define (make-sum a1 a2)
        (cond 
            ((=number? a1 0) a2)
            ((=number? a2 0) a1)
            ((and (number? a1) (number? a2)) (+ a1 a2))
            (else (list a1 '+ a2))
        )
    )

    (define (addend s)
        (car s)
    )

    (define (augend s)
        (cadr s)
    )

    (put 'make-sum '+ make-sum)
    (put 
        'deriv 
        '+ 
        (lambda (exp var)
            (make-sum 
                (deriv (addend exp) var)
                (deriv (augend exp) var)
            )
        )
    )
    'done
)

(define (install-product-package)
    (define (multiplier p)
        (car p)
    )

    (define (multiplicand p)
        (cadr p)
    )

    (define (make-product m1 m2)
        (cond 
            ((or (=number? m1 0) (=number? m2 0)) 0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (number? m1) (number? m2)) (* m1 m2))
            (else (list m1 '* m2))
        )
    )

    (put 'make-product '* make-product)
    (put
        'deriv
        '*
        (lambda (exp var) 
            ((get 'make-sum '+)
                (make-product
                    (multiplier exp)
                    (deriv (multiplicand exp) var)
                )
                (make-product
                    (deriv (multiplier exp) var)
                    (multiplicand exp)
                )
            )
        )
    )
)

(define (install-exponentiation-package)
    (define (base e)
        (car e)
    )

    (define (exponent e)
        (cadr e)
    )

    (define (make-exponentiation b e)
        (cond 
            ((= e 0) 1)
            ((= e 1) b)
            (else (list '** b e))
        )
    )

    (put
        'deriv
        '**
        (lambda (exp var) 
            (define make-product (get 'make-product '*))

            (make-product
                (exponent exp)
                (make-product
                    (make-exponentiation 
                        (base exp)
                        (- (exponent exp) 1)
                    )
                    (deriv (base exp) var)
                )
            )
        )
    )
)

(install-sum-package)
(install-product-package)
(install-exponentiation-package)
