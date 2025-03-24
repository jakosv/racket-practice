#lang racket

(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))

(define (task-4 tree h)
  (cond ((empty-tree? tree) (= h 0))
        (else (and (task-4 (tree-left tree) (- h 1))
             (task-4 (tree-right tree) (- h 1))
        ))))

(task-4 #() 0)
(task-4 #(1 #() #()) 1)
(task-4 #() 1)
