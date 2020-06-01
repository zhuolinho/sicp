; (define (memo-proc proc)
;     (let ((already-run? false) (result false))
;         (lambda () 
;             (if (not already-run?)
;                 (begin
;                     (set! result (proc))
;                     (set! already-run? true)
;                     result)
;                 result))))

; (define (delay exp)
;     (memo-proc (lambda () exp)))

; (define (force delayed-object)
;     (delayed-object))

; (define (cons-stream a b)
;     (cons a (delay b)))

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

; (define (stream-map proc s)
;     (if (stream-null? s)
;         the-empty-stream
;         (cons-stream
;             (proc (stream-car s))
;             (stream-map proc (stream-cdr s)))))

(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
            (apply proc (map stream-car argstreams))
            (apply stream-map (cons
                proc
                (map stream-cdr argstreams))))))

; (define x (stream-map show (stream-enumerate-interval 0 10)))
; (stream-ref x 5)

(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin
            (proc (stream-car s))
            (stream-for-each proc (stream-cdr s)))))

(define (display-line x)
    (newline)
    (display x))

(define (display-stream s)
    (stream-for-each display-line s))

; (define one-to-ten (stream-enumerate-interval 1 10))
; (stream-map + one-to-ten one-to-ten)
; (display-stream (stream-map + one-to-ten one-to-ten))

(define (stream-filter pred stream)
    (cond 
        ((stream-null? stream)
            the-empty-stream)
        ((pred (stream-car stream)) 
            (cons-stream
                (stream-car stream)
                (stream-filter pred (stream-cdr stream))))
        (else 
            (stream-filter pred (stream-cdr stream)))))

(define sum 0)
(define (accum x)
    (set! sum (+ x sum))
    sum)
(define seq (stream-map accum
    (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter
    (lambda (x) (= (remainder x 5) 0))
    seq))

(stream-ref y 7)
(display-stream z)