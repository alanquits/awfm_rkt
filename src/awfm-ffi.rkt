#lang racket

(require ffi/unsafe
         ffi/unsafe/define)

(provide W)

(define-ffi-definer define-awfm (ffi-lib "libawfm.dll"))

(define-awfm W (_fun _double -> _double))
(define-awfm K0 (_fun _double -> _double))
(define-awfm Factorial (_fun _int -> _double))