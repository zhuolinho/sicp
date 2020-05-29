(define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (cons-stream
            low
            (stream-enumerate-interval (+ low 1) high))))

(define (show x)
    (display x)
    x)

(define (stream-map proc s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream
            (proc (stream-car s))
            (stream-map proc (stream-cdr s)))))

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)