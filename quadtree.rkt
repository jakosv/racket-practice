#lang racket

(define empty-tree #())
(define white-tree 0)
(define black-tree 1)
(define make-tree vector)
(define (tree-up-left tree) (vector-ref tree 0))
(define (tree-up-right tree) (vector-ref tree 1))
(define (tree-down-left tree) (vector-ref tree 2))
(define (tree-down-right tree) (vector-ref tree 3))
(define (empty-tree? t) (equal? t #()))
(define (white-tree? t) (equal? t 0))
(define (black-tree? t) (equal? t 1))

(define (taskII t s)
  (cond ((empty-tree? t) 0)
        ((white-tree? t) 0)
        ((black-tree? t) s)
        (else (let ((p (/ s 4)))
          (+ (taskII (tree-up-left t) p)
             (taskII (tree-up-right t) p)
             (taskII (tree-down-left t) p)
             (taskII (tree-down-right t) p)
          )
        ))
  )
)

(taskII #() 16)
(taskII 0 16)
(taskII 1 16)
(taskII #(1 0 0 #(1 1 0 1)) 16)

(define (taskIII-cps t s cc)
  (cond ((empty-tree? t) (cc 0))
        ((white-tree? t) (cc 0))
        ((black-tree? t) (cc s))
        (else (let ((p (/ s 4)))
          (taskIII-cps (tree-up-left t) p 
              (lambda (y1) (taskIII-cps (tree-up-right t) p 
                (lambda (y2) (taskIII-cps (tree-down-left t) p
                  (lambda (y3) (taskIII-cps (tree-down-right t) p
                    (lambda (y4) (cc (+ y1 y2 y3 y4))))))))
          ))
        ))
  )
)

(taskIII-cps #() 16 identity)
(taskIII-cps 0 16 identity)
(taskIII-cps 1 16 identity)
(taskIII-cps #(1 0 0 #(1 1 0 1)) 16 identity)

;; (lambda (y1) (taskII-cps (tree-up-right t) p 
;;                 (lambda (y2) (taskII-cps (tree-down-left t) p
;;                   (lambda (y3) (taskII-cps (tree-down-right t) p
;;                     (lambda (y4) (cc (+ y1 y2 y3 y4)))))))
;; )
;;
;; (lambda (y2) (taskII-cps (tree-down-left t) p
;;                 (lambda (y3) (taskII-cps (tree-down-right t) p
;;                   (lambda (y4) (cc (+ y1 y2 y3 y4))))))
;; )
;;
;; (lambda (y3) (taskII-cps (tree-down-right t) p
;;                 (lambda (y4) (cc (+ y1 y2 y3 y4)))))
;;
;; (lambda (y4) (cc (+ y1 y2 y3 y4)))

