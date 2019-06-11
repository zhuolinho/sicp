(define (product term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* result (term a)))
        )
    )
    (iter a 1)
)

(define (factorial n)
    (product (lambda (n) n)  
        1
        (lambda (n) (+ n 1)) 
        n
    )
)

(define (pi n)
    (define (numer_term i)
        (- 1 (/ 1 (+ (* i 2) 1)))
    )
    (define (denom_term i)
        (+ 1 (/ 1 (+ (* i 2) 1)))
    )
    (define (inc i) (+ i 1))
    (* 4 (product numer_term 1 inc n) (product denom_term 1 inc n))
)
