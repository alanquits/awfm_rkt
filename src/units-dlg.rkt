#lang racket/gui

(require framework/gui-utils)

(provide units-dlg-open)

(define (dummy-callback button event)
  '())

(define (units-dlg-open parent model)
  (define dlg (new dialog% [parent parent] [label "Units Dialog"]))
  (gui-utils:ok/cancel-buttons dlg dummy-callback dummy-callback) 
  dlg)