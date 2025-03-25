#lang racket

(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))

(define (task-5 tree h)
  (call/cc (lambda (cc-exit)
    (let helper ((tree tree) (h h))
      (cond ((empty-tree? tree) (= h 0))
            ((not (helper (tree-left tree) (- h 1))) (cc-exit #f))
            ((not (helper (tree-right tree) (- h 1))) (cc-exit #f))
            (else (cc-exit #t))
      )
    )
  ))
)

(task-5 #() 0)
(task-5 #(1 #() #()) 1)
(task-5 #() 1)
