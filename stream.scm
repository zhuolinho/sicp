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

; (stream-ref y 7)
; (display-stream z)

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
    (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define s (cons-stream 1 (add-streams s s)))

(define (mul-streams s1 s2)
    (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams factorials integers)))

(define (partial-sums s)
    (define self
        (cons-stream (stream-car s) 
            (add-streams self (stream-cdr s))))
    self)

(define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor)) stream))

(define (merge s1 s2)
    (cond 
        ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (let ((s1car (stream-car s1)) (s2car (stream-car s2)))
            (cond
                ((< s1car s2car)
                    (cons-stream s1car (merge (stream-cdr s1) s2)))
                ((> s1car s2car)
                    (cons-stream s2car (merge s1 (stream-cdr s2))))
                (else (cons-stream s1car
                    (merge (stream-cdr s1) (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

(stream-head S 40000)
