#lang racket

(define (funVI lst res num)
       (if (null? lst)
           res
           (funVI (cdr lst)
                   (if (= 0 (modulo num (car lst)))
                       res
                       (* res (car lst))
                       ) num)
           ))

(define (power n p)
  (cond ((= 1 p) n)
        ((= 0 (modulo p 2))
            (let ((res (power n (/ p 2))))
                  (* res res)))
        (else (* n (power n (- p 1))))))

(define (perfect n)
  (let ((k (+ n 1)))
        (* (power 2 (- k 1)) (- (power 2 k) 1))))

(funVI (list 5 10) 1 10)
(power 2 3)
(perfect 11)