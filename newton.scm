(load "fixed-point.scm")

(define dx 0.00001)

(define (deriv g)
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx))
)

(define (newton_transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x))))
)

(define (newtons-method g guess)
    (fixed-point (newton_transform g) guess)
)

(define (sqrt x)
    (newtons-method (lambda (y) (- (square y) x)) 1.0)
)

(define (cubic a b c)
    (lambda (x) (+ (cube x) (* a (square x)) (* b x) c))
)

(define (cube x)
    (* x x x)
)
