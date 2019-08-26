(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))
        )
    )
)

(define (enumerate-interval low high)
    (if (> low high)
        `()
        (cons low (enumerate-interval (+ low 1) high))
    )
)

(define (flatmap proc seq)
    (accumulate append `() (map proc seq))
)

(define (permutations s)
    (if (null? s)
        (list `())
        (flatmap
            (lambda (x)
                (map (lambda (p) (cons x p))
                    (permutations (remove x s))
                )
            )
            s
        )
    )
)

(define (remove item sequence)
    (filter (lambda (x) (not (= x item)))
        sequence
    )
)

(define (unique-pairs n)
    (flatmap 
        (lambda (x) 
            (map (lambda (y) (list x y)) 
                (enumerate-interval 1 (- x 1))
            )
        ) 
        (enumerate-interval 2 n)
    )
)

(define (trible-pairs n)
    (flatmap
        (lambda (x) 
            (flatmap 
                (lambda (y) 
                    (map
                        (lambda (z) (list z y x))
                        (enumerate-interval 1 (- y 1))
                    )
                )
                (enumerate-interval 2 (- x 1))
            )
        )
        (enumerate-interval 3 n)
    )
)

(define (sum lst)
    (accumulate + 0 lst)
)

(define (eqoal-sum n)
    (filter
        (lambda (lst) (= (sum lst) n))
        (trible-pairs n)
    )
)

(define empty-board
    (list)
)

(define (adjoin-position new k lst)
    (cons new lst)
)

; (define (cal p1 p2)
;     (and (not (= (cdr p1) (cdr p2))) (not (= (- (car p1) (car p2)) (abs (- (cdr p1) (cdr p2))))))
; )

; (define (dna a b)
;     (and a b)
; )

(define (safe? k positions)
    (define (iter pos lst num)
        (cond ((null? lst) #t)
            ((or (= pos (car lst)) (= num (abs (- pos (car lst))))) #f)
            (else (iter pos (cdr lst) (+ num 1)))
        )
    )
    (iter (car positions) (cdr positions) 1)
)

(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter (lambda (positions) (safe? k positions))
                (flatmap
                    (lambda (rest-of-queens) 
                        (map (lambda (new-row) (adjoin-position new-row k rest-of-queens))
                            (enumerate-interval 1 board-size)
                        )
                    )
                    (queen-cols (- k 1))
                )
            )
        )
    )
    (queen-cols board-size)
)
