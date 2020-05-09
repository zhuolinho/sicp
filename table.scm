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
    (define (make-entry k v) (cons k v))
    (define (key e) (car e))
    (define (value e) (cdr e))
    (define (set-value! e v) (set-cdr! e v))

    (define (adjoin-set x v set)
        (if (null? set)
            (make-tree (make-entry x v) '() '())
            (let ((result (compare x (key (entry set)))))
                (cond
                    ((= 0 result)
                        (set-value! (entry set) v)
                        set)
                    ((< 0 result)
                        (make-tree (entry set)
                            (adjoin-set x v (left-branch set))
                            (right-branch set)))
                    (else
                        (make-tree (entry set)
                            (left-branch set)
                            (adjoin-set x v (right-branch set))))))))
    
    (define (lookup-set k set)
        (if (null? set)
            false
            (let ((result (compare k (key (entry set)))))
                (cond 
                    ((= 0 result) (value (entry set)))
                    ((< 0 result) (lookup-set k (left-branch set)))
                    (else (lookup-set k (right-branch set)))))))

    (let ((local-table (list)))
        (define (lookup key)
            (lookup-set key local-table))

        (define (insert! key value)
            (set! local-table (adjoin-set key value local-table))
            'ok)

        (define (dispatch m)
            (cond 
                ((eq? m 'lookup) lookup)
                ((eq? m 'insert!) insert!)
                ((eq? m 'table) local-table)
                (else (error "Unknow operation -- TABLE" m))))

        dispatch))

(define t (make-table compare-symbol))
((t 'insert!) 'peter 10086)
((t 'insert!) 'b 'b)
((t 'insert!) 'a 'a)
((t 'insert!) 'z 'z)
((t 'insert!) 'f 'f)
((t 'insert!) 'd 'd)
((t 'insert!) 'e 'e)
((t 'insert!) 'p 'p)
((t 'insert!) 'g 'g)
((t 'insert!) 's 's)
((t 'insert!) 'f 'abc)
((t 'lookup) 'f)
