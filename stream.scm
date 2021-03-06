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

(define n-ones (cons-stream -1 n-ones))

(define (integrate-series series)
    (define (iter stream n)
        (cons-stream (/ (stream-car stream) n)
            (iter (stream-cdr stream) (+ n 1))))
    (iter series 1))

(define exp-series
    (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
    (cons-stream 1 (mul-streams (integrate-series sine-series) n-ones)))

(define sine-series
    (cons-stream 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
    (cons-stream (* (stream-car s1) (stream-car s2))
        (add-streams
            (mul-series s1 (stream-cdr s2))
            (scale-stream (stream-cdr s1) (stream-car s2)))))

(define one (add-streams
    (mul-series cosine-series cosine-series)
    (mul-series sine-series sine-series)))

(define (reciprocal-series s)
    (define self
        (cons-stream 1
            (scale-stream (mul-series
                self
                (stream-cdr s))
                -1)))
    self)

(define (div-series s1 s2)
    (let ((factor (stream-car s2)))
        (if (= factor 0)
            (error "factor is 0")
            (mul-series s1
                (scale-stream
                    (reciprocal-series (scale-stream s2
                        (/ 1 factor)))
                    (/ 1 factor))))))

(define tan (div-series (scale-stream sine-series 2) (scale-stream cosine-series 2)))

(define (sqrt x tolerance)
    (stream-limit (sqrt-stream x) tolerance))

(define (sqrt-stream x)
    (define guesses
        (cons-stream 1.0 
            (stream-map
                (lambda (guess)
                    (sqrt-improve guess x))
                guesses)))
    guesses)

(define (sqrt-improve guess x)
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y)
       2))

(define (stream-limit s tolerance)
    (if 
        (<
            (abs (- (stream-car s) (stream-car (stream-cdr s))))
            tolerance)
        (stream-car (stream-cdr s))
        (stream-limit (stream-cdr s) tolerance)))

(define (ln-summands n)
    (cons-stream (/ 1.0 n)
        (stream-map - (ln-summands (+ n 1)))))

(define ln-stream
    (partial-sums (ln-summands 1)))

(define (euler-transform s)
    (let ((s0 (stream-ref s 0)) (s1 (stream-ref s 1)) (s2 (stream-ref s 2)))
        (cons-stream
            (- s2
                (/ (square (- s2 s1))
                    (+ s0 (* -2 s1) s2)))
            (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
    (cons-stream s
        (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
    (stream-map stream-car
        (make-tableau transform s)))

(define (interleave s1 s2)
    (if (stream-null? s1)
        s2
        (cons-stream (stream-car s1)
            (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
    (cons-stream (list (stream-car s) (stream-car t))
        (interleave 
            (stream-map
                (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
            (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
    (cons-stream (list (stream-car s) (stream-car t) (stream-car u))
        (interleave 
            (stream-map
                (lambda (x) (cons (stream-car s) x))
                (stream-cdr (pairs t u)))
            (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (merge-weighted s1 s2 proc)
    (cond 
        ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (let ((s1car (stream-car s1)) (s2car (stream-car s2)))
            (let ((weight (proc s1car s2car)))
                (cond
                    ((< weight 0)
                        (cons-stream s1car (merge-weighted (stream-cdr s1) s2 proc)))
                    ((> weight 0)
                        (cons-stream s2car (merge-weighted s1 (stream-cdr s2) proc)))
                    (else (cons-stream s1car
                        (merge-weighted (stream-cdr s1) (stream-cdr s2) proc)))))))))

(define (weighted-pairs s t proc)
    (cons-stream (list (stream-car s) (stream-car t))
        (merge-weighted 
            (stream-map
                (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
            (weighted-pairs (stream-cdr s) (stream-cdr t) proc)
            proc)))

(define abc (lambda (a b)
    (let ((sum1 (+ (car a) (cadr a))) (sum2 (+ (car b) (cadr b))))
        (cond 
            ((< sum1 sum2) -1)
            (else 1)))))

(define (def a b)
    (define (cal lst)
        (let ((i (car lst)) (j (cadr lst)))
            (+ (* 2 i) (* 3 j) (* 5 i j))))

    (let ((sum1 (cal a)) (sum2 (cal b)))
        (cond
            ((< sum1 sum2) -1)
            (else 1))))

; (stream-head 
;     (stream-filter
;         (lambda (lst)
;             (define (remain c)
;                 (or 
;                     (= (remainder c 2) 0)
;                     (= (remainder c 3) 0)
;                     (= (remainder c 5) 0)))
;             (or (remain (car lst)) (remain (cadr lst))))
;         (weighted-pairs integers integers def))
;     128)

(define (weighted-triples s t u proc1 proc2)
    (cons-stream (list (stream-car s) (stream-car t) (stream-car u))
        (merge-weighted 
            (stream-map
                (lambda (x) (cons (stream-car s) x))
                (stream-cdr (weighted-pairs t u proc2)))
            (weighted-triples (stream-cdr s) (stream-cdr t) (stream-cdr u) proc1 proc2)
            proc1)))

(define pythagoras (stream-filter 
    (lambda (lst) (= 
        (+ (square (car lst)) (square (cadr lst))) 
        (square (caddr lst))))
    (weighted-triples integers integers integers
        (lambda (a b)
            (if (< (+ (car a) (cadr a) (caddr a)) (+ (car b) (cadr b) (caddr b)))
                -1
                1))
        abc)))

; (display-stream pythagoras)

(define (cube-sum lst)
    (+ (cube (car lst)) (cube (cadr lst))))

(define ramanujan (weighted-pairs integers integers (lambda (a b)
    (let ((sum1 (cube-sum a)) (sum2 (cube-sum b)))
        (cond 
            ((< sum1 sum2) -1)
            (else 1))))))

; (stream-for-each (lambda (x) #f) ramanujan)

(define (square-sum lst)
    (+ (square (car lst)) (square (cadr lst))))

(define trico (weighted-pairs integers integers (lambda (a b)
    (if (< (square-sum a) (square-sum b))
        -1
        1))))

(define (stream-each proc s)
    (if (stream-null? s)
        'done
        (begin
            (proc s)
            (stream-each proc (stream-cdr s)))))

; (stream-each
;     (lambda (s)
;         (if (= (square-sum (stream-car s)) (square-sum (stream-car (stream-cdr s))) (square-sum (stream-car (stream-cdr (stream-cdr s)))))
;             (begin
;                 (display-line (stream-car s))
;                 (display (stream-car (stream-cdr s)))
;                 (display (stream-car (stream-cdr (stream-cdr s)))))
;             #f))
;     trico)

; (stream-each
;     (lambda (s) 
;         (if (= (cube-sum (stream-car s)) (cube-sum (stream-car (stream-cdr s))))
;             (display-line (cube-sum (stream-car s)))
;             #f))
;     ramanujan)

; (define (integral integrand initial-value dt)
;     (define int (cons-stream initial-value
;         (add-streams
;             (scale-stream integrand dt)
;             int)))
;     int)

; (define (RC R C dt)
;     (lambda (i v0) 
;         (add-streams
;             (scale-stream i R)
;             (integral (scale-stream i (/ 1 C)) v0 dt))))

; (define RC1 (RC 5 1 0.5))

; (stream-ref (RC1 integers 1) 10)

(define (integral delayed-integrand initial-value dt)
    (cons-stream initial-value
        (let ((integrand (force delayed-integrand)))
            (if (stream-null? integrand)
                the-empty-stream
                (integral (delay (stream-cdr integrand))
                    (+
                        (* dt (stream-car integrand))
                        initial-value)
                    dt)))))

(define (solve f y0 dt)
    (define y (integral (delay dy) y0 dt))
    (define dy (stream-map f y))
    y)

(define (solve-2nd f y0 dy0 dt)
    (define y (integral (delay dy) y0 dt))
    (define dy (integral (delay ddy) dy0 dt))
    (define ddy (stream-map f dy y))
    y)

; (stream-ref (solve-2nd (lambda (a b) (+ (* -6 a) (* 7 b))) 1 1 0.001) 1000)

(define (RLC R C L dt)
    (lambda (vc0 il0)
        (define vc (integral (delay dvc) vc0 dt))
        (define il (integral (delay dil) il0 dt))
        (define dvc (scale-stream il (/ -1 C)))
        (define dil (add-streams
            (scale-stream vc (/ 1 L))
            (scale-stream il (- (/ R L)))))

        (cons vc il)))

(define RC2 (RLC 1 0.2 1 0.1))

(define (rand-update seed)
    (remainder (+ (* seed 9301) 49297) 233280))

(define random-numbers
    (cons-stream 0
        (stream-map rand-update random-numbers)))

