#lang racket

; without tail recursion, so memory usage - O(N)
(define (fact n)
  (if (< n 2) 1
      (* n (fact (- n 1)))
  )
)

; with tail recursion, so memory usage - O(1)
(define (iter_fact n)
  (define (loop i res)
    (if (< i 2) res
      (loop (- i 1) (* i res))
      )
  )
  (loop n 1)
)

(define (iter_fact2 n)
  (let loop ((i n) (res 1))
    (if (< i 2) res
      (loop (- i 1) (* i res))
    )
  )
)

; factorial
(fact 4)
(iter_fact 4)
(iter_fact2 4)


; complexity - O(n!), memory - O(n) (tree height)
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))
  )
)

; complexity - O(n), memory - O(const)
(define (iter_fib n)
  (define (loop i fib_n-2 fib_n-1)
    (if (= i 2) (+ fib_n-1 fib_n-2)
        (loop (- i 1) fib_n-1 (+ fib_n-1 fib_n-2))
    )
  )
  (loop n 0 1)
)

(define (iter_fib2 n)
  (let loop ((i n) (fib_n-2 0) (fib_n-1 1))
    (if (= i 2) (+ fib_n-1 fib_n-2)
        (loop (- i 1) fib_n-1 (+ fib_n-1 fib_n-2))
    )
  )
)

; fibonaci
(fib 10)
(iter_fib 10)
(iter_fib2 10)


; list reverse
(define (recursive-reverse lst)
  (if (null? lst) '()
      (append (recursive-reverse (cdr lst)) (list (car lst)))
  )
)

(define (iter-reverse lst)
  (define (loop old new)
    (if (null? old) new
        (loop (cdr old) (cons (car old) new))
    )
  )
  (loop lst '())
)

(define (iter-reverse2 lst)
  (let loop ((old lst) (new '()))
    (if (null? old) new
        (loop (cdr old) (cons (car old) new))
    )
  )
)

;recursive reverse
(recursive-reverse '(1 2 3 4 5))
;iterative reverse
(iter-reverse '(1 2 3 4 5))
(iter-reverse2 '(1 2 3 4 5))