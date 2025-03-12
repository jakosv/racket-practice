#lang racket

(define (sum-squares a b)
  (if (> a b)
      0
      (+ (* a a) (sum-squares (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (* a a a) (sum-cubes (+ a 1) b))))



(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (sum-squares2 a b)
  (sum (lambda (x) (* x x)) a (lambda (x) (+ x 1)) b))

(define (sum-cubes2 a b)
  (sum (lambda (x) (* x x x)) a add1 b))

(sum-squares 1 4)
(sum-cubes 1 4)
(sum-squares2 1 4)
(sum-cubes2 1 4)


; without recursion
(define (iter-sum term a next b)
  (let loop ((a a) (result 0))
    (if (> a b) result
        (loop (next a) (+ result (term a))))))

(define (sum-squares3 a b)
  (iter-sum (lambda (x) (* x x)) a add1 b))

(define (sum-cubes3 a b)
  (iter-sum (lambda (x) (* x x x)) a add1 b))


(sum-squares3 1 4)
(sum-cubes3 1 4)


(define (product term a next b)
  (let loop ((a a) (result 1))
    (if (> a b) result
        (loop (next a) (* result (term a))))))

(define (product-squares a b)
  (product (lambda (x) (* x x)) a add1 b))

(product-squares 1 3)


(define (accumulate combiner null-val term a next b)
  (let loop ((a a) (result null-val))
    (if (> a b) result
        (loop (next a) (combiner (term a) result)))))

(define (sum-square4 a b)
  (accumulate + 0 (lambda (x) (* x x)) a add1 b))

(define (sum-cubes4 a b)
  (accumulate + 0 (lambda (x) (* x x x)) a add1 b))

(define (product-square4 a b)
  (accumulate * 1 (lambda (x) (* x x)) a add1 b))

(define (product-cubes4 a b)
  (accumulate * 1 (lambda (x) (* x x x)) a add1 b))

(sum-square4 1 4)
(sum-cubes4 1 4)
(product-square4 1 3)
(product-cubes4 1 3)

(define (enumerate a b)
  (accumulate (lambda (x y) (append y x)) '() list a add1 b))

(define (reverse-enumerate a b)
  (reverse (accumulate cons '() (lambda (x) x) a add1 b)))

; O(n^2) because of append
(enumerate 1 10)

; O(n)
(reverse-enumerate 1 10)

