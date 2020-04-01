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
    (define (match? type1 type-list)
        (if (null? type-list)
            true
            (let ((type2 (car type-list)))
                (if (or (equal? type1 type2) (get-coercion type2 type1))
                    (match? type1 (cdr type-list))
                    false
                )
            )
        )
    )
    (define (convert type1 arg-list)
        (if (null? arg-list)
            '()
            (let ((type2 (type-tag (car arg-list))))
                (if (equal? type1 type2)
                    (cons (car arg-list) (convert type1 (cdr arg-list)))
                    (cons ((get-coercion type2 type1) (car arg-list)) (convert type1 (cdr arg-list)))
                )
            )
        )
    )
    (define (available-type type type-left type-right)
        (cond 
            (match? type (append type-left type-right) type)
            ((null? type-right) false)
            (else (available-type (car type-right) (cons type type-left) (cdr type-right)))
        )
    )
    (define (same-type? type-list)
        (cond 
            ((null? type-list) true)
            ((null? (cdr type-list)) true)
            ((equal? (car type-list) (cadr type-list)) (same-type? (cdr type-list)))
            (else false)
        )
    )
    (define (find-available type type-left type-right)
        (let ((type (available-type type type-left type-right)))
            (cond 
                ((not type) type)
                ((get op (map (lambda (_) type) args)) type)
                ((null? type-right) false)
                (else (find-available (car type-right) (cons type type-left) (cdr type-right)))
            )
        )
    )
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (cond 
                (proc (apply proc (map contents args)))
                ((same-type? type-tags) (error "No method for equal types" (list op type-tags)))
                (else
                    (let ((type (find-available (car type-tags) '() (cdr type-tags))))
                        (if type
                            (apply apply-generic (cons op (convert type args)))
                            (error "No method for these types" (list op type-tags))
                        )
                    )
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
    'done
)
(define (make-scheme-number n)
    ((get 'make 'scheme-number) n)
)

(define (install-rational-package)
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (define (make-rat n d)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))
        )
    )
    (define (add-rat x y)
        (make-rat
            (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))
        )
    )
    (define (sub-rat x y)
        (make-rat
            (- (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))
        )
    )
    (define (mul-rat x y)
        (make-rat
            (* (numer x) (numer y))
            (* (denom x) (denom y))
        )
    )
    (define (div-rat x y)
        (make-rat
            (* (numer x) (denom y))
            (* (denom x) (numer y))
        )
    )

    (define (tag x)
        (attach-tag 'rational x)
    )
    (put 'add '(rational rational)
        (lambda (x y) (tag (add-rat x y)))
    )
    (put 'sub '(rational rational)
        (lambda (x y) (tag (sub-rat x y)))
    )
    (put 'mul '(rational rational)
        (lambda (x y) (tag (mul-rat x y)))
    )
    (put 'div '(rational rational)
        (lambda (x y) (tag (div-rat x y)))
    )
    (put 'make 'rational
        (lambda (n d) (tag (make-rat n d)))
    )
    'done
)
(define (make-rational n d)
    ((get 'make 'scheme-number) n d)
)

(define (install-complex-package)
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (magnitude z)
        (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
    (define (angle z)
        (atan (imag-part z) (real-part z)))
    ;;; imported procedures from rectangular and polar packages
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y))

    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang 'polar) r a))

    ;;; interal procedures
    (define (add-complex z1 z2)
        (make-from-real-imag (+ (real-part z1) (real-part z2))
                             (+ (imag-part z1) (imag-part z2))))

    (define (sub-complex z1 z2)
        (make-from-real-imag (- (real-part z1) (real-part z2))
                             (- (imag-part z1) (imag-part z2))))

    (define (mul-complex z1 z2)
        (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                           (+ (angle z1) (angle z2))))

    (define (div-complex z1 z2)
        (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                           (- (angle z1) (angle z2))))

    ;;; interface to rest of the system
    (define (tag z)
        (attach-tag 'complex z))

    (put 'add '(complex complex)
        (lambda (z1 z2)
            (tag (add-complex z1 z2))))

    (put 'sub '(complex complex)
        (lambda (z1 z2)
            (tag (sub-complex z1 z2))))

    (put 'mul '(complex complex)
        (lambda (z1 z2)
            (tag (mul-complex z1 z2))))

    (put 'div '(complex complex)
        (lambda (z1 z2)
            (tag (div-complex z1 z2))))

    (put 'make-from-real-imag 'complex
        (lambda (x y)
            (tag (make-from-real-imag x y))))

    (put 'make-from-mag-ang 'complex
        (lambda (r a)
            (tag (make-from-mag-ang r a))))

'done)
(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y)
)
(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a)
)


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(install-scheme-number-package)
(apply-generic 'div (make-scheme-number 2) (make-scheme-number 3))
