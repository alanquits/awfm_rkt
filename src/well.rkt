#lang racket

(require "utility.rkt")
(require "transientparameter.rkt")

(define well-class%
  (class object%
    (init-field
     [name "well name"] ; Name of well 
     [x 0]              ; X coordinate
     [y 0]              ; Y coordinate
     [rw 1.0]           ; Radius of well
     [h0 (make-object transient-parameter-linear% 0 0)] ; Initial water level
     [c (make-object transient-parameter-linear% 0 0)]  ; Turbulant well loss
     [b (make-object transient-parameter-linear% 0 0)]  ; Laminar well loss
     )
    
    (define/public (distance-to other-x other-y)
      (max rw (distance-xy x y other-x other-y)))

    (super-new)))