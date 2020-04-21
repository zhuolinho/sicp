(define (front-ptr queue)
    (car queue))

(define (rear-ptr queue)
    (cdr queue))

(define (set-front-ptr! queue item)
    (set-car! queue item))

(define (set-rear-ptr! queue item)
    (set-cdr! queue item))

(define (empty-queue? queue)
    (null? (front-ptr queue)))

(define (make-queue)
    (cons '() '()))

(define (front-queue queue)
    (if (empty-queue? queue)
        (error "Front called with an empty queue" queue)
        (car (front-ptr queue))))

(define (insert-queue! queue item)
    (let ((new-pair (cons item '())))
        (cond 
            ((empty-queue? queue)
                (set-front-ptr! queue new-pair)
                (set-rear-ptr! queue new-pair)
                queue)
            (else 
                (set-cdr! (rear-ptr queue) new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))

(define (delete-queue! queue)
    (cond 
        ((empty-queue? queue) 
            (error "DELETE! called with an empty queue" queue))
        (else 
            (set-front-ptr! queue (cdr (front-ptr queue)))
            queue)))

(define (print-queue queue)
    (car queue))

(define (make-queue)
    (let ((front-ptr '()) (rear-ptr '()))
        (define (empty-queue?)
            (null? front-ptr))

        (define (front-queue)
            (if (empty-queue?)
                (error "Front called with an empty queue")
                (car front-ptr)))
        
        (define (insert-queue! item)
            (let ((new-pair (list item)))
                (cond 
                    ((empty-queue?)
                        (set! front-ptr new-pair)
                        (set! rear-ptr new-pair)
                        front-ptr)
                    (else 
                        (set-cdr! rear-ptr new-pair)
                        (set! rear-ptr new-pair)
                        front-ptr))))

        (define (delete-queue!)
            (cond 
                ((empty-queue?) (error "DELETE! called with an empty queue"))
                (else 
                    (set! front-ptr (cdr front-ptr))
                    front-ptr)))

        (define (dispatch m)
            (cond 
                ((eq? m 'insert-queue!) insert-queue!)
                ((eq? m 'delete-queue!) (delete-queue!))
                ((eq? m 'empty-queue?) (empty-queue?))
                ((eq? m 'front-queue) (front-queue))
                ((eq? m 'print-queue) front-ptr)
                (else (error "Unknow operation -- DISPATCH" m))))

        dispatch))

(define q1 (make-queue))
((q1 'insert-queue!) 'a)
((q1 'insert-queue!) 'b)
(q1 'delete-queue!)
((q1 'insert-queue!) 'c)
(q1 'print-queue)