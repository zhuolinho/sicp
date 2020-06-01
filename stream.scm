(define (memo-proc proc)
    (let ((already-run? false) (result false))
        (lambda () 
            (if (not already-run?)
                (begin
                    (set! result (proc))
                    (set! already-run? true)
                    result)
                result))))

(define (delay exp)
    (memo-proc (lambda () exp)))

(define (force delayed-object)
    (delayed-object))

(define (cons-stream a b)
    (cons a (delay b)))

(define (stream-car stream)
    (car stream))

(define (stream-cdr stream)
    (force (cdr stream)))

(define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

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