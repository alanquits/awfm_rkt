#lang racket

(provide distance-xy
         vector-insert)

(define (sum-of-squares x1 x2)
  (+ (* x1 x1) (* x2 x2)))

(define (distance-xy x1 y1 x2 y2)
  (let ([dx (- x2 x1)]
        [dy (- y2 y1)])
    (sqrt (sum-of-squares dx dy))))

(define (vector-insert v idx value)
  (let-values ([(fst lst) (vector-split-at v idx)])
    (vector-append fst (vector value) lst)))
