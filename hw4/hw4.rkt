
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file
;; put your code below

;1
(define sequence
  (lambda (low high stride)
    (if (> low high)
        null
        (cons low (sequence (+ low stride) high stride)))))

;2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;3
(define (list-nth-mod xs n)
  (define i (remainder n (length xs)))
  (cond [(< n 0) (error  "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs i))]))

;4
(define (stream-for-n-steps s n)
  (letrec ([next (s)])
  (if (= n 0)
      null
      (cons (car next) (stream-for-n-steps (cdr next) (- n 1))))))

;5
(define funny-number-stream
  (letrec ([f (lambda (x)
    (cond [(= (remainder x 5) 0) (cons (- x) (lambda () (f (+ x 1))))]
          [#t (cons x (lambda () (f (+ x 1))))]))])
  (lambda () (f 1))))

;6
(define (dan-then-dog)
  (define (f x) (cond [(= x 1) (cons "dan.jpg" (lambda () (f 0)))]
                      [(= x 0) (cons "dog.jpg" (lambda () (f 1)))]))
  (f 1))

;7
(define (stream-add-zero s)
  (letrec ([f (lambda (s)
                (letrec ([s_ans (s)])
                (cons (cons 0 (car s_ans)) (lambda () (f (cdr s_ans))))))])
  (lambda () (f s))))

;8
(define (cycle-lists xs ys)
  (define (f n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))))
  (lambda () (f 0)))

;9
(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [f (lambda (vec n)
                  (if (>= n len)
                      #f
                      (letrec ([ans (vector-ref vec n)])
                      (cond
                        [(pair? ans) (cond
                                       [(equal? (car ans) v) ans]
                                       [#t (f vec (+ n 1))])]
                        [#t (f vec (+ n 1))]))))])
  (f vec 0)))
                    
;10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [next 0]
           [xs-len (length xs)])
  (lambda (v)
    (let ([ans_cached (vector-assoc v cache)]) 
      (cond [ans_cached ans_cached]
            [#t (let ([ans (assoc v xs)])
                  (if ans
                      (begin
                        (vector-set! cache next ans)
                        (if (< (+ next 1) xs-len)
                            (set! next (+ next 1))
                            (set! next 0))
                        ans)
                      #f))])))))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     "skipped"]))
