(define (count-leaves x)
    (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x)) (count-leaves (cdr x))))
    )
)

; (define x (list 1 3 (list 5 7) 9))

; (define y (list (list 7)))

; (define z (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

; (car (cdaddr x))
; (caar y)
; (cadadr (cadadr (cadadr z)))

; (define x (list (list 1 2) (list 3 4)))
; (define y (list x x))

(define (deep-reverse lst)
    (define (iter lst result)
        (if (null? lst)
            result
            (iter (cdr lst) (cons (deep-reverse (car lst)) result))
        )
    )
    (if (pair? lst)
        (iter lst '())
        lst
    )
)

(define (fringe tree)
    (cond ((null? tree) tree)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree)) (fringe (cdr tree))))
    )
)

(define (make-mobile left right)
    (cons left right)
)

(define (make-branch length structure)
    (cons length structure)
)

(define (left-branch mobile)
    (car mobile)
)

(define (right-branch mobile)
    (cdr mobile)
)

(define (branch-length branch)
    (car branch)
)

(define (branch-structure branch)
    (cdr branch)
)

(define (total-weight mobile)
    (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile)))
)

(define (branch-weight branch)
    (if (pair? (branch-structure branch))
        (total-weight (branch-structure branch))
        (branch-structure branch)
    )
)

(define (branch-torque branch)
    (* (branch-weight branch) (branch-length branch))
)

(define (mobile-balance? mobile)
    (let ((left (left-branch mobile))
        (right (right-branch mobile)))
        (and (= (branch-torque left) (branch-torque right))
            (branch-balance? left)
            (branch-balance? right)
        )
    )
)

(define (branch-balance? branch)
    (if (pair? (branch-structure branch))
        (mobile-balance? (branch-structure branch))
        #t
    )
)
