#lang racket

(require "aquiferdrawdown.rkt")
(require "definitions.rkt")
(require "utility.rkt")

(provide model%)

; Returns initial model flags
(define (init-model-flags)
  (let ([h (make-hash)])
    (hash-set! h "well-loss-turbulant-on" #f)
    (hash-set! h "well-loss-laminar-on" #f)
    (hash-set! h "well-loss-transient-on" #f)
    (hash-set! h "h0-transient-on" #f)
    h))

; Returns initial model units
(define (init-model-units)
  (let ([h (make-hash)])
    (hash-set! h "length" "meters")
    (hash-set! h "time" "days")
    (hash-set! h "discharge" "m3/day")
    h))

(define model%
  (class object%
    (init-field [wells (vector)]
                [aquifer-drawdown-model (new theis-drawdown% [S 0.01] [T 100])]
                [flags (init-model-flags)]
                [units (init-model-units)])

    ; insert well
    (define/public (insert-well w idx)
      (set! wells (vector-insert wells idx w)))

    (define/public (append-well w)
      (set! wells (vector-append wells (vector w))))

    ; flag getter/setter
    (define/public (set-flag flag is-on)
      (if (hash-has-key? flags flag)
          (hash-set! flags flag is-on)
          '()))

    (define/public (get-flag flag)
      (hash-ref flags flag '()))

    ; unit getter/setter
    (define (set-unit-if-allowable unit-type unit allowable-units)
      (if (member unit allowable-units)
          (hash-set! units unit-type unit)
          '()))
    
    (define/public (set-unit unit-type unit)
      (if (hash-has-key? units unit-type)
          (cond [(string=? unit-type "length")
                 (set-unit-if-allowable unit-type unit allowable-length-units)]
                [(string=? unit-type "time")
                 (set-unit-if-allowable unit-type unit allowable-time-units)]
                [(string=? unit-type "discharge")
                 (set-unit-if-allowable unit-type unit allowable-discharge-units)])
          '()))

    (define/public (get-unit unit-type)
      (hash-ref units unit-type '()))
    
    (super-new)))

(define m (new model%))