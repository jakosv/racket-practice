#lang racket

(define (my_append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (my_append (cdr lst1) lst2)) 
  )
)

(define (my_reverse lst)
  (if (null? lst)
      '()
      (my_append (my_reverse (cdr lst)) (list (car lst)))
  )
)

(define (my_apply op lst)
  (if (null? (cdr lst))
      (car lst)
      (op (car lst) (my_apply op (cdr lst)))
   )
)

(my_append '(1 2 3) '(4 5))
(my_reverse '(1 2 3 4 5))
(my_apply + '(1 2 3))