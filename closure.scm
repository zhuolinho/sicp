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

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)