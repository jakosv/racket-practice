#lang racket

; DEFINE USER DATA TYPES
(println "USER DATA TYPES")

(define (make-rat a b)
  (let ((g (gcd a b))) (cons (/ a g) (/ b g)))
)

(define num car)
(define den cdr)

(define (sum-rat a b)
  (make-rat (+ (* (num a) (den b)) (* (den a) (num b))) (* (den a) (den b)))
)

(define (mul-rat a b)
  (make-rat (* (num a) (num b)) (* (den a) (den b)))
)

(define rat_num1 (make-rat 4 12))
(num rat_num1)
(den rat_num1)

(define rat_num2 (make-rat 15 25))
(num rat_num2)
(den rat_num2)

(sum-rat rat_num1 rat_num2)
(mul-rat rat_num1 rat_num2)


(define (my_cons x y)
  (define (matcher e)
    (cond ((= e 0) x)
          ((= e 1) y)
          (else (error "cons error"))
    )
  )
matcher)

(define (my_car x) (x 0))
(define (my_cdr x) (x 1))

(my_cons 'a 'b)
(define pair (my_cons 'a 'b))
(my_car pair)
(my_cdr pair)


; VECTORS
(println "VECTORS")

(define v (vector 'a 'b 'c 'd))
(println v)
(vector-ref v 3)
(vector-length v)
(vector? v)
(vector->list v)
(list->vector '(a b c d))


; BINARY TREES
(println "BINARY TREES")

; BINARY TREE IMPLEMETED WITH PAIRS
(println "BINARY TREE PAIRS IMPLEMETATION")

(define empty-tree '())
(define (make-tree data left right)
  (cons data (cons left right))
)
(define tree-data car)
(define tree-left cadr)
(define tree-right cddr)
(define empty-tree? null?)

(make-tree 1 (make-tree 2 empty-tree empty-tree)
             (make-tree 3 empty-tree empty-tree)
)

; BINARY TREE IMPLEMENTES WITH VECTORS
(println "BINARY TREE IMPLEMENTED WITH VECTOR")

(define empty-t #())
(define make-t vector)
(define (t-data tree) (vector-ref tree 0))
(define (t-left tree) (vector-ref tree 1))
(define (t-right tree) (vector-ref tree 2))
(define (empty-t? tree) (equal? tree #()))

(eq? empty-t #())
(eqv? empty-t #())
(equal? empty-t #())

(make-t 1 (make-t 2 empty-t empty-t) (make-t 3 empty-t empty-t))

; BINARY SEARCH TREE

(define empty-bst empty-tree)
(define empty-bst? empty-tree?)

(define (member-bst? key t)
  (cond ((empty-bst? t) #f)
        ((= key (tree-data t)) #t)
        ((< key (tree-data t))
         (member-bst? key (tree-left t)))
        (else
          (member-bst? key (tree-right t)))
  )
)

(define (insert-bst key t)
  (cond ((empty-bst? t)
          (make-tree key empty-bst empty-bst))
        ((= key (tree-data t)) t)
        ((< key (tree-data t))
          (make-tree (tree-data t) 
                      (insert-bst key (tree-left t))
                      (tree-right t))
        )
        (else
          (make-tree (tree-data t) 
                      (tree-left t)
                      (insert-bst key (tree-right t)))
        )
  )
)

(define t (insert-bst 1 (insert-bst 4 (insert-bst 3 empty-bst))))
(println t)
(member-bst? 4 t)
