(define (make-accumulator sum)
    (lambda (x) (set! sum (+ sum x)) sum))

(define A (make-accumulator 5))

(define (make-monitored f)
    (let ((times 0))
        (lambda (x) 
            (cond ((eq? x 'how-many-calls?) times)
                ((eq? x 'reset-count) (set! times 0))
                (else
                    (set! times (+ times 1))
                    (f x))))
    )
)
(define s (make-monitored sqrt))

(define (make-account balance password)
    (let ((times 0))
        (define (withdraw amount)
            (if (>= balance amount)
                (begin
                    (set! balance (- balance amount))
                    balance)
                "Insufficient funds"))
        (define (deposit amount)
            (set! balance (+ balance amount))
            balance)
        (define (check-password f pwd)
            (lambda (amount) 
                (if (eq? pwd password)
                    (begin
                        (set! times 0)
                        (f amount))
                    (begin
                        (set! times (+ times 1))
                        (if (>= times 7)
                            (error "You try too much times, calling the cops ...")
                            (display "Incorrect password")
                        )
                        times))))
        (define (dispatch pwd m)
            (cond ((eq? m 'withdraw) (check-password withdraw pwd))
                ((eq? m 'deposit) (check-password deposit pwd))
                (else (error "Unknow request -- MAKE-ACCOUNT" m))))
        dispatch
    ))
(define (make-joint account old-pwd new-pwd)
    (lambda (pwd m)
        (if (eq? new-pwd pwd)
            (account old-pwd m)
            display-wrong-another-password-message)
    ))

(define (display-wrong-another-password-message _)
    (display "Incorrect another password"))
(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (random (exact->inexact range)))))

(define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
        (cond 
            ((= trials-remaining 0) (/ trials-passed trials))
            ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
            (else (iter (- trials-remaining 1) trials-passed))))
    (iter trials 0))

(define (p x y)
    (< (+ (* x x) (* y y)) 1))

(define (estimate-integral process x1 x2 y1 y2 trials)
    (monte-carlo trials (lambda () 
        (p (random-in-range x1 x2) (random-in-range y1 y2))
    )))

(define (fn)
    (let ((last 0))
        (lambda (x) (set! last x))))
(define f (fn))
(+ (f 0) (f 1))

(define (make-cycle x)
    (set-cdr! (last-pair x) x)
    x)

(define (count-pairs x)
    (define (inner y memo-list)
        (cond 
            ((not (pair? y)) memo-list)
            ((memq y memo-list) memo-list)
            (else 
                (inner (car y) 
                    (inner (cdr y) (cons y memo-list)))
            )))
    (length (inner x '())))

(define x '((a) b c))
(count-pairs (cons x x))