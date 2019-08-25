(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))
        )
    )
)

; (define (map p sequence)
;     (accumulate
;         (lambda (x y)
;             (cons (p x) y)
;         )
;         `()
;         sequence
;     )
; )

(define (append seq1 seq2)
    (accumulate cons seq2 seq1)
)

(define (length sequence)
    (accumulate (lambda (x y) (+ y 1)) 0 sequence)
)

(define (horner-eval x coefficient-sequence)
    (accumulate 
        (lambda (this-coeff higher-terms)
            (+ (* x higher-terms) this-coeff)
        )
        0
        coefficient-sequence
    )
)

(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        (car seqs)
        (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs))
        )
    )
)

(define (count-leaves t)
    (accumulate + 0
        (map
            (lambda (tree)
                (if (pair? tree)
                    (count-leaves tree)
                    1
                )
            ) 
            t
        )
    )
)

(define (doc-product v w)
    (accumulate + 0 (map * v w))
)

(define (matrix-*-vector m v)
    (map 
        (lambda (w) 
            (doc-product v w)
        ) 
        m
    )
)

(define (transpose mat)
    (accumulate-n cons `() mat)
)

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (x)
                (matrix-*-vector cols x)
            )
            m
        )
    )
)

(define m 
    (list 
        (list 1 2 3 4)
        (list 4 5 6 6)
        (list 6 7 8 9)
    )
)

(define v (list 1 2 3 4))

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest))
                (cdr rest)
            )
        )
    )
    (iter initial sequence)
)

(define (reverse sequence)
    (fold-left (lambda (x y)
            (cons y x)
        )
        `()
        sequence
    )
)

(define (reverse sequence)
    (accumulate 
        (lambda (x y)
            (append y (list x))
        )
        `()
        sequence
    )
)
