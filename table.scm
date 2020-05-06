(define (make-table same-key?)
    (define (assoc key records)
        (cond 
            ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (let ((local-table (list '*table*)))
        (define (lookup key-1 key-2)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (cdr record)
                            false))
                    false)))

        (define (insert! key-1 key-2 value)
            (let ((subtable (assoc key-1 (cdr local-table))))
                (if subtable
                    (let ((record (assoc key-2 (cdr subtable))))
                        (if record
                            (set-cdr! record value)
                            (set-cdr! subtable
                                (cons
                                    (cons key-2 value)
                                    (cdr subtable)))))
                    (set-cdr! local-table
                        (cons
                            (list
                                key-1
                                (cons key-2 value))
                            (cdr local-table)))))
            'ok)

        (define (dispatch m)
            (cond 
                ((eq? m 'lookup-proc) lookup)
                ((eq? m 'insert-proc!) insert!)
                (else (error "Unknow operation -- TABLE" m))))

        dispatch))

(define (lookup key-1 key-2 table)
    (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
                (if record
                    (cdr record)
                    false))
            false)))

(define (insert! key-1 key-2 value table)
    (let ((subtable (assoc key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
                (if record
                    (set-cdr! record value)
                    (set-cdr! subtable
                        (cons
                            (cons key-2 value)
                            (cdr subtable)))))
            (set-cdr! table
                (cons
                    (list key-1 (cons key-2 value))
                    (cdr table)))))
    'ok)

(define (make-table)
    (list '*table*))

(define t (make-table))
(insert! 'abc 'cde 123 t)
(lookup 'abc 'cde t)