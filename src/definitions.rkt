#lang racket

(provide allowable-length-units
         allowable-time-units
         allowable-discharge-units)

(define allowable-length-units '("meters" "feet" "yards"))

(define allowable-time-units '("days" "minutes" "seconds"))

(define allowable-discharge-units '("m3/day" "ft3/day" "gal/min"))