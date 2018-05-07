#lang racket/gui

(require framework/gui-utils)

(provide awfm:gui:units-dlg%)

(define awfm:gui:units-dlg%
  (class dialog%
    (super-new)

    (init-field accept-callback units-hash)

    (define panel (new vertical-panel% [parent this]
                       [vert-margin 10] [horiz-margin 10]))

    (define length-choice (new choice%
                               (label "Length:   ") (parent panel)
                               (choices (list "meters" "yards" "feet"))
                               [vert-margin 3] [horiz-margin 10] [min-width 200]))
    (send length-choice set-string-selection (hash-ref units-hash "length" "meters"))

    (define time-choice (new choice%
                               (label "Time:   ") (parent panel)
                               (choices (list "days" "minutes" "seconds"))
                               [vert-margin 3] [horiz-margin 10] [min-width 200]))
    (send time-choice set-string-selection (hash-ref units-hash "time" "days"))

    (define discharge-choice (new choice%
                               (label "Discharge:   ") (parent panel)
                               (choices (list "m3/day" "ft3/day" "gal/min"))
                               [vert-margin 3] [horiz-margin 10] [min-width 200]))
    (send discharge-choice set-string-selection (hash-ref units-hash "discharge" "m3/day"))


    (define button-box (new horizontal-panel% [parent panel]
                            [alignment (list 'center 'center)]
                            [vert-margin 3]))
    
    (gui-utils:ok/cancel-buttons button-box
                                 accept-callback
                                 (lambda (b e) (send this show #f)))

    (define/public (update-model m)
      (send* m
        (set-unit "length" (send length-choice get-string-selection))
        (set-unit "time" (send time-choice get-string-selection))
        (set-unit "discharge" (send discharge-choice get-string-selection)))
      m)

    ))
   