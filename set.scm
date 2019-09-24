(define (union-set set1 set2)
    (cond 
        ((null? set1) set2)
        ((element-of-set? (car set1) set2) 
            (union-set (cdr set1) set2)
        )
        (else 
            (cons
                (car set1)
                (union-set (cdr set1) set2)
            )
        )
    )
)

(define (element-of-set? x set)
    (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))
    )
)

(define (adjoin-set x set)
    (cond ((null? set) (cons x set))
        ((> x (car set)) (cons (car set) (addjoin-set x (cdr set))))
        ((< x (car set)) (cons x set))
        (else set)
    )
)

(define (union-set set1 set2)
    (cond 
        ((null? set1) set2)
        ((null? set2) set1)
        (else 
            (let ((x1 (car set1)) (x2 (car set2)))
                (cond 
                    ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                    ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                    ((> x1 x2) (cons x2 (union-set set1 (cdr set2))))
                )
            )
        )
    )
)
