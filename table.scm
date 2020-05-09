(define (lookup keys table)
    ; (let ((subtable (assoc key-1 (cdr table))))
    ;     (if subtable
    ;         (let ((record (assoc key-2 (cdr subtable))))
    ;             (if record
    ;                 (cdr record)
    ;                 false))
    ;         false)))
    (if (list? keys)
        (if (null? keys)
            (cdr table)
            (let ((subtable (assoc (car keys) (cdr table))))
                (if subtable
                    (lookup (cdr keys) subtable)
                    false)))
        (lookup (list keys) table)))

(define (insert! keys value table)
    ; (let ((subtable (assoc key-1 (cdr table))))
    ;     (if subtable
    ;         (let ((record (assoc key-2 (cdr subtable))))
    ;             (if record
    ;                 (set-cdr! record value)
    ;                 (set-cdr! subtable
    ;                     (cons
    ;                         (cons key-2 value)
    ;                         (cdr subtable)))))  
    ;         (set-cdr! table
    ;             (cons
    ;                 (list key-1 (cons key-2 value))
    ;                 (cdr table)))))
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

(define (entry tree)
    (car tree))

(define (left-branch tree)
    (cadr tree))

(define (right-branch tree)
    (caddr tree))

(define (make-tree entry left right)
    (list entry left right))

(define (compare-string x y)
    (cond ((string=? x y)
            0)
          ((string>? x y)
            1)
          ((string<? x y)
            -1)))

(define (compare-symbol x y)
    (compare-string (symbol->string x)
                    (symbol->string y)))

(define (make-table compare)
    (define (adjoin-set x set)
        (let ((result (compare x (entry set))))
            (cond 
                ((= 0 result) set)
                ((= -1 result)
                    (make-tree (entry set)
                        (adjoin-set x (left-branch set))
                        (right-branch set)))
                ((= 1 result)
                    (make-tree (entry set)
                        (left-branch set)
                        (adjoin-set x (right-branch set)))))))

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

(define t (make-table compare-symbol))
