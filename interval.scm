(define (make-interval a b)
    (cons a b)
)

(define (lower-bound z)
    (car z)
)

(define (upper-bound z)
    (cdr z)
)

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y)) (+ (upper-bound x) (upper-bound y)))
)

(define (sub-interval x y)
    (add-interval x (make-interval (- (upper-bound y)) (- (lower-bound y))))
)

(define (mul-interval x y)
(let* ((lx (lower-bound x))
(ux (upper-bound x))
(ly (lower-bound y))
(uy (upper-bound y))
(pos-lx? (positive? lx))
(pos-ux? (positive? ux))
(pos-ly? (positive? ly))
(pos-uy? (positive? uy)))
(cond
; lx ux ly uy example
; ----------------------------------
; + - + + invalid interval
; + - + - invalid interval
; + - - + invalid interval
; + - - - invalid interval
((and pos-lx? (not pos-ux?))
(error "invalid interval" x))

; + + + - invalid interval
; - + + - invalid interval
; - - + - invalid interval
((and pos-ly? (not pos-uy?))
(error "invalid interval" y))

; + + + + (1.2)(2.3) = (2.6)
((and pos-lx? pos-ux? pos-ly? pos-uy?)
(make-interval (* lx ly) (* ux uy)))

; + + - + (1.2)(-2.3) = (-4.6)
((and pos-lx? pos-ux? (not pos-ly?) pos-uy?)
(make-interval (* ux ly) (* ux uy)))

; + + - - (1.2)(-2.-1) = (-4.-1)
((and pos-lx? pos-ux? (not pos-ly?) (not pos-uy?))
(make-interval (* ux ly) (* lx uy)))

; - + + + (-1.2)(2.3) = (-3.6)
((and (not pos-lx?) pos-ux? pos-ly? pos-uy?)
(make-interval (* lx uy) (* ux uy)))

; - + - + (-1.2)(-2.3) = (-4.6) *
((and (not pos-lx?) pos-ux? (not pos-ly?) pos-uy?)
(make-interval (min (* lx uy) (* ux ly))
(max (* ux uy) (* lx ly))))

; - + - - (-1.2)(-2.-1) = (-4.2)
((and (not pos-lx?) pos-ux? (not pos-ly?) (not pos-uy?))
(make-interval (* ux ly) (* lx ly)))

; - - + + (-2.-1)(2.3) = (-6.-2)
((and (not pos-lx?) (not pos-ux?) pos-ly? pos-uy?)
(make-interval (* lx uy) (* ux ly)))

; - - - + (-2.-1)(-2.3) = (-6, 4)
((and (not pos-lx?) (not pos-ux?) (not pos-ly?) pos-uy?)
(make-interval (* lx uy) (* lx ly)))

; - - - - (-2.-1)(-2.-1) = (1.4)
((and (not pos-lx?) (not pos-ux?) (not pos-ly?) (not pos-uy?))
(make-interval (* ux uy) (* lx ly))))))

(define (div-interval x y)
    (if (cross-zero? y)
        (error "zero point")
        (mul-interval x 
            (make-interval (/ 1.0 (upper-bound y))
                (/ 1.0 (lower-bound y))
            )
        )
    )
)

(define (cross-zero? z)
    (and (< (lower-bound z) 0) (>= (upper-bound z) 0))
)

(define (make-center-width c w)
    (make-interval (- c w) (+ c w))
)

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2)
)

(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2)
)

(define (make-center-percent c p)
    (make-center-width c (* c p))
)

(define (percent i)
    (/ (width i) (center i))
)

(define a (make-center-percent 1 1))

(define b (make-center-percent 3 0.5))
