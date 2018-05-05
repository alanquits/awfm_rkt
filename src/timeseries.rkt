#lang racket

(struct measure (t v) #:transparent)

(define (measure-set-minimum-magnitude m magnitude)
  (if (< (abs (measure-v m)) (abs magnitude))
      (measure (measure-t m) 0)
      m))

(define (measure-sign m)
  (let ([v (measure-v m)])
    (cond [(positive? v) 1]
          [(negative? v) -1]
          ((zero? v) 0))))

(define (measure-enforce-range-constraint m min-value max-value)
  (if (< (measure-v m) min-value)
      (measure (measure-t m) min-value)
      (if (> (measure-v m) max-value)
          (measure (measure-t m) max-value)
          m)))

(define (measure-scale-value m s)
  (measure (measure-t m) (* s (measure-v m))))

(define (measure-scale-time m s)
  (measure (* s (measure-t m)) (measure-v m)))

; Calculates the derivative/slope between two measures (dv/dt)
(define (measure-slope m0 mf)
  (let ([t0 (measure-t m0)]
        [v0 (measure-v m0)]
        [tf (measure-t mf)]
        [vf (measure-v mf)])
    (/ (- vf v0) (- tf t0))))

; Given two measures m0 and mf that bound a time t, return a new
; measure with a linearly-interpolated value at time t.
(define (measure-linear-interpolate m0 mf t)
  (let ([m (measure-slope m0 mf)]
        [dt (- t (measure-t m0))]
        [v0 (measure-v m0)])
    (measure t (+ v0 (* m dt)))))

; Reduce timeseries size using bandwidth
(define (timeseries-average-by-bandwidth ms bw)
  
  (define (ok-to-collect prev-min prev-max this-value)
    (let ([this-min (min prev-min this-value)]
          [this-max (max prev-max this-value)])
      (< (- this-max this-min) bw)))
  
  (define (split-by-bandwidth receiving-list providing-list prev-min prev-max)
    (if (and (not (empty? providing-list))
             (ok-to-collect prev-min prev-max (measure-v (car providing-list))))
        (let ([head (car providing-list)])
          (split-by-bandwidth (cons head receiving-list) (cdr providing-list)
                              (min (measure-v head) prev-min)
                              (max (measure-v head) prev-max)))
          (values (list->vector (reverse receiving-list)) (list->vector providing-list))))

  (define (timeseries-average-by-bandwidth-iter grouped-measures remaining-ms)
    (if (= (vector-length remaining-ms) 0)
        grouped-measures
        (let ([init-measure-value (measure-v (vector-ref remaining-ms 0))])
          (let-values ([(fst lst)
                        (split-by-bandwidth '() (vector->list remaining-ms) init-measure-value init-measure-value)])
            (timeseries-average-by-bandwidth-iter (cons (timeseries-mean fst) grouped-measures) lst)))))
      
  (if (<= (vector-length ms) 1)
      ms
      (list->vector (reverse (timeseries-average-by-bandwidth-iter '() ms)))))
    

; Reduce timeseries size using sign changes
(define (timeseries-average-by-sign ms)
  (define (timeseries-next-sign-change-index ms)
  (define returned-idx -1)
  (if (> (vector-length ms) 1)
      (let ([sign (measure-sign (vector-ref ms 0))])
        (define time-to-leave #f)
        (for ([idx (range (vector-length ms))]) #:break time-to-leave
          (if (not (= (measure-sign (vector-ref ms idx)) sign))
              (begin
                (set! returned-idx idx)
                (set! time-to-leave #t))
              '())))
      '())
    returned-idx)
  
  (define (timeseries-average-by-sign-rec ms components)
    (let ([idx (timeseries-next-sign-change-index ms)])
      (if (= idx -1)
          (reverse (map timeseries-mean (cons ms components)))
          (begin
            (let-values ([(fst lst) (vector-split-at ms idx)])
              (timeseries-average-by-sign-rec lst (cons fst components)))))))
  (timeseries-average-by-sign-rec ms '()))

; Removes adjacent measures in time series that have the same value. This is useful for reducing
; pumping rate data. If this function has performance problems, consider pre-allocating 'ms-returned'
; to size of 'ms' and trim off the extraneous values on return.
(define (timeseries-consolidate-adjacent-values ms)
  (define ms-returned (vector-take ms 1))
  (if (<= (vector-length ms) 1)
      ms
      (begin
        (let ([init (vector-drop-right ms 1)]
              [last (vector-drop ms 1)])
          (for ([x1 init] [x2 last])
            (if (= (measure-v x1) (measure-v x2))
                '()
                (set! ms-returned (vector-append ms-returned (vector x2))))))
        ms-returned)))

; Returns length of timeseries
(define (timeseries-length ms)
  (vector-length ms))

; Returns maximum value in timeseries
(define (timeseries-max ms)
  (measure-v (vector-argmax measure-v ms)))

; Averages the values in a time series. Time is assigned to the first measure time
; in the series.
(define (timeseries-mean ms)
  (if (= (vector-length ms) 0)
      '()
      (measure (measure-t (vector-ref ms 0))
               (/ (apply + (vector->list (timeseries-values ms)))
                  (vector-length ms)))))

; Returns minimum value in timeseries
(define (timeseries-min ms)
  (measure-v (vector-argmin measure-v ms)))

; Converts any values in a timeseries with an absolute value less than 'magnitude' to 0
(define (timeseries-set-minimum-magnitude ms magnitude)
  (vector-map (lambda (m) (measure-set-minimum-magnitude m magnitude)) ms))

; Returns a vector of signs (-1, 0, 1) depending on whether a measure values are positive,
; negative, or zero.
(define (timeseries-sign ms)
  (vector-map measure-sign ms))

; Converts any number less than 'min-value' to 'min-value' and any number greater than
; 'max-value' to 'max-value' in a timeseries
(define (timeseries-enforce-range-constraint ms min-value max-value)
  (vector-map (lambda (m) (measure-enforce-range-constraint m min-value max-value)) ms))

; Remove any value from timeseries equal to 'value'. This is useful for removing error
; codes from a dataset.
(define (timeseries-remove-by-value ms value)
  (vector-filter (lambda (m) (not (= (measure-v m) value))) ms))

; Scales times in timeseries. This is useful for unit conversions
(define (timeseries-scale-time ms s)
  (vector-map (lambda (m) (measure-scale-time m s)) ms))

; Scales values in timeseries. This is useful for unit conversions
(define (timeseries-scale-value ms s)
  (vector-map (lambda (m) (measure-scale-value m s)) ms))

              
;
(define (timeseries-values ms)
  (vector-map (lambda (m) (measure-v m)) ms))

(define vs (vector (measure 0 10.0) (measure 1 15.0) (measure 2 60.0) (measure 3 0.0) (measure 4 10.0) (measure 5 10.0) (measure 6 0.0) (measure 7 -40.0)))