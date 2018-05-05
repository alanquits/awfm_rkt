#lang racket

(provide transient-parameter-linear%)

(define transient-parameter-interface<%>
  (interface () value-at-t))

(define transient-parameter-linear%
  (class* object% (transient-parameter-interface<%>)
    (init-field intercept
                slope)
    (define/public (value-at-t t)
      (+ (* slope t) + intercept))

    (define/public (set-slope new-slope)
      (set! slope new-slope))

    (define/public (set-intercept new-intercept)
      (set! intercept new-intercept))
    
    (super-new)))