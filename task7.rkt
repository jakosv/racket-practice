#lang racket

(require scheme/mpair)

; уместнее использовать реализацию функцией, так как мы можем менять
; элементы списка, в силу того, что копия списка в функции указывает
; на те же самые объекты в памяти. Поэтому смысл в создании макроса
; ради изменения состояния глобального по отношению к функции объекта отпадает

; function realisation
(define (rot-left! mlst)
  (define (loop lst m0)
       (cond ((null? lst) (error "empty list"))
             ((null? (mcdr lst)) (set-mcar! lst m0))
             (else (begin (set-mcar! lst (mcar (mcdr lst))) (loop (mcdr lst) m0)))
       )
  )
  (loop mlst (mcar mlst))
)

(define lst (mlist 1 2 3))
(rot-left! lst)
lst

; macros realisation
(define-syntax rot-left-macros!
  (syntax-rules ()
    ((_ mlst) (begin
      (define (loop lst m0)
       (cond ((null? lst) (error "empty list"))
             ((null? (mcdr lst)) (set-mcar! lst m0))
             (else (begin (set-mcar! lst (mcar (mcdr lst))) (loop (mcdr lst) m0)))
       )
      )
      (loop mlst (mcar mlst)))
    )
  )
)

(set! lst (mlist 1 2 3))
(rot-left-macros! lst)
lst