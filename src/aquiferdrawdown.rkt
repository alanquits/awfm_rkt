#lang racket

(require "awfm-ffi.rkt")

(provide theis-drawdown%)

(define aquifer-drawdown-interface<%>
  (interface () drawdown-at-point name))

(define theis-drawdown%
  (class* object% (aquifer-drawdown-interface<%>)
    (super-new)
    (init-field S T)
    
    (define (calc-u r t)
      (/ (* (* r r) S) (* 4 T t)))
    
    (define/public (drawdown-basic r Q t)
      (let ([u (calc-u r t)])
        (* (/ Q (* 4 pi T)) (W u))))
    
    (define/public (drawdown-at-point ws x y t)
      0)

    (define/public (name) "theis")
    ))