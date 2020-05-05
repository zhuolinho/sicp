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

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 'a 'b 1)
(put 'a 'c 2)
(put 'b 'c 3)
(get 'a 'c)