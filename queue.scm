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

(define (make-element item)
    (cons item '()))

(define (get-item ele)
    (car ele))

(define (pre ele)
    (cdr ele))

(define (set-pre! ele1 ele2)
    (set-cdr! ele1 ele2))

(define (make-deque)
    (cons (list) (list)))

(define (empty-deque? deque)
    (null? (front-ptr deque)))

(define (front-deque deque)
    (if (empty-deque? deque)
        (error "Front called with an empty deque" deque)
        (car (front-ptr deque))))

(define (rear-deque deque)
    (if (empty-deque? deque)
        (error "Rear called with an empty deque" deque)
        (car (rear-ptr deque))))

(define (front-insert-deque! deque item)
    (let ((element (make-element item)))
        (cond 
            ((empty-deque? deque) 
                (let ((new-pair (cons element '())))
                    (set-front-ptr! deque new-pair)
                    (set-rear-ptr! deque new-pair)
                    (print-deque deque)))
            (else 
                (let ((front (cons element (front-ptr deque))))
                    (set-pre! (front-deque deque) front)
                    (set-front-ptr! deque front)
                    (print-deque deque))))))

(define (rear-insert-deque! deque item)
    (let ((new-pair (cons (make-element item) '())))
        (cond 
            ((empty-deque? deque)
                (set-front-ptr! deque new-pair)
                (set-rear-ptr! deque new-pair)
                (print-deque deque))
            (else 
                (set-pre! (car new-pair) (rear-ptr deque))
                (set-cdr! (rear-ptr deque) new-pair)
                (set-rear-ptr! deque new-pair)
                (print-deque deque)))))

(define (front-delete-deque! deque)
    (cond 
        ((empty-deque? deque) 
            (error "DELETE! called with an empty deque" deque))
        (else 
            (set-front-ptr! deque (cdr (front-ptr deque)))
            (set-pre! (front-deque deque) '())
            (print-deque deque))))

(define (rear-delete-deque! deque)
    (cond 
        ((empty-deque? deque) 
            (error "DELETE! called with an empty deque" deque))
        (else 
            (let ((rear (pre (rear-deque deque))))
                (set-cdr! rear '())
                (set-rear-ptr! deque rear)
                (print-deque deque)))))

(define (print-deque deque)
    (map get-item (car deque)))

(define q (make-deque))
(front-insert-deque! q 1)
(rear-insert-deque! q 2)
(front-delete-deque! q)
(rear-insert-deque! q 3)
(rear-delete-deque! q)
(front-insert-deque! q 4)
(rear-delete-deque! q)
(rear-insert-deque! q 5)