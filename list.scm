(define nil (quote ()))

(define (last-pair li)
    (if (null? (cdr li))
        li
        (last-pair (cdr li))
    )
)

(define (reverse lst)
    (cond ((null? lst) lst)
        (else (cons (reverse (cdr lst)) (car lst)))
    )
)

(define (construct ls end)
    (if (null? ls)
        end
        (construct (car ls) (cons (cdr ls) end))
    )
)

; (construct (reverse (list 149 34)) nil)

(define (reset lst)
    (define (iter list tmp)
        (if (null? list)
            tmp
            (iter (cdr list) (cons (car list) tmp))
        )
    )
    (iter lst nil)
)

(define (append list1 list2)
    (define (iter lst1 lst2)
        (if (null? lst1)
            lst2
            (iter (cdr lst1) (cons (car lst1) lst2))
        )
    )
    (iter (reset list1) list2)
)

(define (same-parity x . l)
    ; (define (iter lst end)
    ;     (cond ((null? lst) end)
    ;         ((even? (+ x (car lst))) (iter (cdr lst) (cons (car lst) end)))
    ;         (else (iter (cdr lst) end))
    ;     )
    ; )
    ; (cons x (iter (reset l) nil))
    (cons x (filter (lambda (y) (even? (+ x y))) l))
)

; (define (sqaure-list items)
;     (if (null? items)
;         nil
;         (cons (* (car items) (car items)) (sqaure-list (cdr items)))
;     )
; )

(define (sqaure-list items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things) (cons answer (sqaure (car things))))
        )
    )
    (iter items nil)
)

(define (sqaure x)
    (* x x)
)

(define (for_each proc items)
    (cond ((null? items) #t)
        (else (proc (car items)) (for_each proc (cdr items)))
    )
)

(for_each (lambda (x) (newline) (display x)) (list 17 23 31 42))
