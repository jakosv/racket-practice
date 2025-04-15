#lang racket

(define (divs_sum n)
  (define (loop i res)
    (cond ((> (* i i) n) res)
          ((= (* i i) n) (+ res i))
          (else 
            (if (= 0 (modulo n i))
              (loop (+ i 1) (+ res i (/ n i)))
              (loop (+ i 1) res)))))
  (loop 2 1)
)

(define (perfect n)
  (define (loop i k)
    (if (= (divs_sum k) k) 
        (if (= i n) k
            (loop (+ i 1) (+ k 2))
        )
        (loop i (+ k 2)))
  )
  (loop 1 2)
)

(divs_sum 496)
(divs_sum 8128)
(perfect 1)
(perfect 2)
(perfect 3)
(perfect 4)


(define (make-table) (make-hash '()))
(define (lookup table key) (hash-ref table key #f))
(define (insert! table key val) (hash-set! table key val))

(define (memoize func)
  (let ((table (make-table)))
    (lambda (x)
      (let ((prev-result (lookup table x)))
        (if prev-result prev-result
          (let ((result (func x)))
            (insert! table x result)
            result))))))

(define memo-perfect (memoize (lambda (n)
    (define (loop i k)
      (if (= (divs_sum k) k) 
          (if (= i n) k
              (loop (+ i 1) (+ k 2))
          )
          (loop i (+ k 2)))
      )
      (loop 1 2)
    ))
)

(memo-perfect 1)
(memo-perfect 2)
(memo-perfect 3)
(memo-perfect 4)
