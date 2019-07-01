; (define (count-change amount)
;     (cc amount 5)
; )

; (define (cc amount kinds-of-coins)
;     (cond ((= amount 0) 1)
;         ((or (< amount 0) (= kinds-of-coins 0)) 0)
;         (else (+ (cc amount (- kinds-of-coins 1)) (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))
;     )
; )

; (define (first-denomination kinds-of-coins)
;     (cond ((= kinds-of-coins 1) 1)
;         ((= kinds-of-coins 2) 5)
;         ((= kinds-of-coins 3) 10)
;         ((= kinds-of-coins 4) 25)
;         ((= kinds-of-coins 5) 50)
;     )
; )

; (count-change 100)

(define us-coins (list 1 25 50 5 10))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
    (cond ((= amount 0) 1)
        ((or (< amount 0) (null? coin-values)) 0)
        (else (+ (cc amount (cdr coin-values)) (cc (- amount (car coin-values)) coin-values)))
    )
)

(cc 100 us-coins)
