(define (make-leaf symbol weight)
    (list 'leaf symbol weight)
)

(define (leaf? object)
    (eq? (car object) 'leaf)
)

(define (symbol-leaf x)
    (cadr x)
)

(define (weight-leaf x)
    (caddr x)
)

(define (left-branch tree)
    (car tree)
)

(define (right-branch tree)
    (cadr tree)
)

(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)
    )
)

(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)
    )
)

(define (make-code-tree left right)
    (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))
    )
)

(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits)
            '()
            (let ((next-branch (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons
                        (symbol-leaf next-branch)
                        (decode-1 (cdr bits) tree)
                    )
                    (decode-1 (cdr bits) next-branch)
                )
            )
        )
    )
    (decode-1 bits tree)
)

(define (choose-branch bit branch)
    (cond 
        ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))
    )
)

(define (encode message tree)
    (if (null? message)
        '()
        (append
            (encode-symbol (car message) tree)
            (encode (cdr message) tree)
        )
    )
)

(define (encode-symbol symbol tree)
    (cond 
        ((leaf? tree) '())
        ((symbol-in-tree? symbol (left-branch tree)) (cons 0 (encode-symbol symbol (left-branch tree))))
        ((symbol-in-tree? symbol (right-branch tree)) (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "This symbol not in tree: " symbol))
    )
)

(define (symbol-in-tree? given-symbol tree)
    (not
        (false?
            (find
                (lambda (s) 
                    (eq? s given-symbol)
                )
                (symbols tree)
            )
        )
    )
)

(define (adjoin-set x set)
    (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
            (adjoin-set x (cdr set)))
        )
    )
)

(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set 
                (make-leaf (car pair) (cadr pair))
                (make-leaf-set (cdr pairs))
            )
        )
    )
)

(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs))
)

(define (successive-merge ordered-set)
    (define (iter left right)
        (if (null? left)
            right
            (iter (cdr left)
                (make-code-tree (car left) right)
            )
        )
    )
    (if (= 0 (length ordered-set))
        '()
        (iter (cdr ordered-set) (car ordered-set))
    )
)

(generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16)))

