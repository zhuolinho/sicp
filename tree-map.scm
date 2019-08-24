(define (square-tree tree)
    (cond ((null? tree) tree)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree)) (square-tree (cdr tree))))
    )
)

(define (square_tree tree)
    (tree-map square tree)
)

(define (tree-map operation tree)
    (map
        (lambda (sub-tree) 
            (if (pair? sub-tree)
                (tree-map operation sub-tree)
                (operation sub-tree)
            )
        )
        tree
    )
)

(define (subsets s)
    (if (null? s)
        (list '())
        (let ((rest (subsets (cdr s))))
            (append rest (map
                (lambda (ele) (cons (car s) ele))
                rest
            ))
        )
    )
)
