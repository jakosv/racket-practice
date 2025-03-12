#lang racket

(define (task2a n)
  (let loop ((i n) (result 0))
    (if (< i 1) result
        (loop (- i 1) (+ (* i (+ i 4)) result)))))

(task2a 1)
(task2a 2)
(task2a 3)
(task2a 0)
(task2a -1)

(define (task2b n)
  (if (< n 1) 0
      (+ (* n (+ n 4)) (task2b (- n 1)))))

(task2b 1)
(task2b 2)
(task2b 3)
(task2b 0)
(task2b -1)
