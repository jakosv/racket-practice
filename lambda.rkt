#lang racket

(lambda (x) (* x x))
((lambda (x) (* x x)) 4)
(define func (lambda (x) (* x x)))
(procedure? func)
(func 4)

(define is_prime
  (lambda (n)
    (case n
      ((2 3 5 7) 'prime)
      ((1 4 6 8) 'composite)
      (else 'unknown)
    )
  )
)

(is_prime (* 2 3))