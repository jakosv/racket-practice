#lang racket

(define (list-fib-squares-a n)
  (let loop ((i n) (fib-n-1 1) (fib-n-2 0) (res 0))
    (if (= i 0) (+ res (* fib-n-2 fib-n-2))
        (loop (- i 1) (+ fib-n-1 fib-n-2) fib-n-1
              (+ res (* fib-n-2 fib-n-2)))))
)

(define (list-fib-squares-b n)
  (define (fib n)
    (define (loop i fib-n-1 fib-n-2)
      (if (= i 0) fib-n-2
          (loop (- i 1) (+ fib-n-1 fib-n-2) fib-n-1))
    )
    (loop n 1 0)
  )
  (foldl (lambda (x y) (+ (* x x) y)) 0 (build-list (+ n 1) fib))
)

(list-fib-squares-a 5)
(list-fib-squares-b 5)
