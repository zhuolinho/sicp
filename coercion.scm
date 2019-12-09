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

(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (= (length args) 2)
                    (let ((type1 (car type-tags))
                          (type2 (cadr type-tags))
                          (a1 (car args))
                          (a2 (cadr args)))
                        (if (equal? type1 type2)
                            (error "No method for equal types" (list op type-tags))
                            (let ((t1->t2 (get-coercion type1 type2))
                                  (t2->t1 (get-coercion type2 type1)))
                                (cond
                                    (t1->t2
                                        (apply-generic op (t1->t2 a1) a2))
                                    (t2->t1
                                        (apply-generic op a1 (t2->t1 a2)))
                                    (else
                                        (error "No method for these types" (list op type-tags)))
                                )
                            )
                        )
                    )
                    (error "Need 2 args" (list op type-tags))
                )
            )
        )
    )
)

(define (attach-tag type-tag contents)
    (cons type-tag contents)
)
(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum -- TYPE-TAG" datum)
    )
)
(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Bad tagged datum -- CONTENTS" datum)
    )
)

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (install-scheme-number-package)
    (define (tag x)
        (attach-tag 'scheme-number x)
    )
    (put 'exp '(scheme-number scheme-number)
        (lambda (x y) (tag (expt x y)))
    )
    (put 'make 'scheme-number
        (lambda (x) (tag x))
    )
    'done
)
(define (make-scheme-number n)
    ((get 'make 'scheme-number) n)
)

(define (scheme-number->scheme-number n) n)
(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)

(install-scheme-number-package)
(apply-generic 'exp (make-scheme-number 3) (make-scheme-number 2))
