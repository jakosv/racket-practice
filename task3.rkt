#lang racket

(define (task3 lst)
  (define (avg v)
    (/ (foldl (lambda (x y) (+ x y)) 0 v) (length v))
  )
  (length (map avg lst))
)

(task3 (list (list 2 4 0) (list 2 2) (list 1 1 1 1 1 1 1)))
