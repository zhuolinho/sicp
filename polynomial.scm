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

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error 
                    "No method for these types -- APPLY-GENERIC"
                    (list op type-tags))))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (install-scheme-number-package)
    (define (tag x)
        (attach-tag 'scheme-number x)
    )
    (put 'add '(scheme-number scheme-number)
        (lambda (x y) (tag (+ x y)))
    )
    (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (tag (- x y)))
    )
    (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (tag (* x y)))
    )
    (put 'div '(scheme-number scheme-number)
        (lambda (x y) (tag (/ x y)))
    )
    (put 'make 'scheme-number
        (lambda (x) (tag x))
    )
    (put '=zero? '(scheme-number)
        (lambda (value) (= value 0))
    )
    'done
)
(define (make-scheme-number n)
    ((get 'make 'scheme-number) n)
)

(define (install-polynomial-package)
    (define (make-poly variable term-list)
        (cons variable term-list))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))

    (define (adjoin-term term term-list)
        (if (=zero? (coeff term))
            term-list
            (cons term term-list)))
    (define (the-empty-termlist) '())
    (define (first-term term-list) (car term-list))
    (define (rest-terms term-list) (cdr term-list))
    (define (empty-termlist? term-list) (null? term-list))
    (define (make-term order coeff) (list order coeff))
    (define (order term) (car term))
    (define (coeff term) (cadr term))

    (define (add-terms L1 L2)
        (cond
            ((empty-termlist? L1) L2)
            ((empty-termList? L2) L1)
            (else (let ((t1 (first-term L1)) (t2 (first-term L2)))
                (cond
                    ((> (order t1) (order t2))
                        (adjoin-term t1
                            (add-terms (rest-terms L1) L2)
                        )
                    )
                    ((< (order t1) (order t2))
                        (adjoin-term t2
                            (add-terms L1 (rest-terms L2))
                        )
                    )
                    (else (adjoin-term
                        (make-term (order t1)
                            (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                            (rest-terms L2))
                    ))
                )
            ))
        )
    )

    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                (add-terms (term-list p1)
                    (term-list p2)))
            (error "Polys not in same var -- ADD-POLY" (list p1 p2)))
    )
    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                (mul-terms (term-list p1)
                    (term-list p2)))
            (error "Polys not in same var -- MUL-POLY" (list p1 p2)))
        )

    (define (tag p) (attach-tag 'polynomial p))
    (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(polynomial polynomial)
        (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'make 'polynomial
        (lambda (var terms) (tag (make-poly var terms))))
'done)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (=zero? x) (apply-generic '=zero? x))

(install-scheme-number-package)
(install-polynomial-package)
(define (make-polynomial var terms)
    ((get 'make 'polynomial) var terms))

(add (make-polynomial 'x (list (list 3 (make-scheme-number 1)) (list 2 (make-scheme-number 5)) (list 1 (make-scheme-number 4)))) 
    (make-polynomial 'x (list (list 5 (make-scheme-number 1)) (list 4 (make-scheme-number 3)) (list 3 (make-scheme-number 2)))))