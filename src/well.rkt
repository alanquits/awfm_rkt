#lang racket

(require "utility.rkt")
(require "transientparameter.rkt")

(define well-class%
  (class object%
    (init-field
     name     ; Name of well 
     x        ; X coordinate
     y        ; Y coordinate
     rw       ; Radius of well
     [h0 (make-object transient-parameter-linear% 0 0)] ; Initial water level
     [c (make-object transient-parameter-linear% 0 0)]  ; Turbulant well loss
     [b (make-object transient-parameter-linear% 0 0)]  ; Laminar well loss
     )
    
    (define/public (distance-to other-x other-y)
      (max rw (distance-xy x y other-x other-y)))

    (super-new)))