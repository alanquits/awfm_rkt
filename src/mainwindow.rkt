#lang racket/gui

(require "model.rkt")
(require "units-dlg.rkt")

(define awfm:gui:mainwindow%
  (class frame%
    (super-new)

    (init-field [model (new model%)]
                [model-is-dirty #t]
                [model-saved-file-name "UNSAVED"])


    (define (init-model-from-file path-to-model)
      (display (format "Opening ~a" path-to-model)))
    
    (define (ok-to-proceed)
      (if model-is-dirty
          (let ([result (message-box "Okay to proceed?"
                       "Current model has unsaved changes. Would you like to proceed?"
                       this
                       (list 'ok-cancel))])
            (if (eq? result 'ok)
                #t
                #f))
          #t))
          

    (define (new-callback button event)
      (if (ok-to-proceed)
          (display "new model")
          (display "not okay ot proceed")))

    (define (open-callback button event)
      (if (ok-to-proceed)
          (let ([path-to-file (get-file "Open model" this)])
            (init-model-from-file path-to-file))
          (void)))

    (define (save-as-callback button event)
      (if (ok-to-proceed)
          (let ([file-name (put-file "Save As" this)])
            (if (path? file-name)
                (begin
                  (display (format "Saving model to ~a" file-name))
                  (set! model-is-dirty #f)
                  (set! model-saved-file-name file-name))
                (void)))
          (void)))
    
    (define (save-callback button event)
      (if (string=? model-saved-file-name "UNSAVED")
          (save-as-callback button event)
          (if (ok-to-proceed)
              (begin
                (display (format "saving model to ~a" model-saved-file-name))
                (set! model-is-dirty #f))
              (void))))
            
    
    (define (dummy-callback button event)
      '())

    (define (quit button event)
      (exit))

    (define (units-dlg-open button event)
      (define (accept-callback button event)
        (send dlg update-model model)
        (send dlg show #f))
      (define dlg (new awfm:gui:units-dlg% [parent this] [label "Units"]
                       [accept-callback accept-callback]
                       [units-hash (get-field units model)]))
      (send dlg show #t))


    (define (wells-dlg-open button event)
      (display "editing wells\n"))
    
    
    (define menu-bar (new menu-bar%
                      (parent this)))

    ; File menu
    (define file-menu (new menu% (label "&File") (parent menu-bar)))
    (new menu-item% [label "&New"] [parent file-menu] [callback new-callback])
    (new menu-item% [label "&Open"] [parent file-menu] [callback open-callback])
    (new separator-menu-item% [parent file-menu])
    (new menu-item% [label "&Save"] [parent file-menu] [callback save-callback])
    (new menu-item% [label "Save &As"] [parent file-menu] [callback save-as-callback])
    (new separator-menu-item% [parent file-menu])
    (new menu-item% [label "&Quit"] [parent file-menu] [callback quit])

    ; Model menu
    (define model-menu (new menu% (label "&Model") (parent menu-bar)))
    (new menu-item% [label "&Units"] [parent model-menu] [callback units-dlg-open])
    (new menu-item% [label "&Wells"] [parent model-menu] [callback wells-dlg-open])
    (new menu-item% [label "&Pumping Rates"] [parent model-menu] [callback dummy-callback])
    (new menu-item% [label "&Aquifer Drawdown"] [parent model-menu] [callback dummy-callback])
    (new menu-item% [label "&Well Loss"] [parent model-menu] [callback dummy-callback])
    (new separator-menu-item% [parent model-menu])
    (new menu-item% [label "&Run"] [parent model-menu] [callback dummy-callback])
    (new menu-item% [label "&Export Results"] [parent model-menu] [callback dummy-callback])

    ; Parameter Estimation Menu
    (define pest-menu (new menu% [label "&Parameter Estimation"] [parent menu-bar]))
    (new menu-item% [label "&Settings"] [parent pest-menu] [callback dummy-callback])
    (new menu-item% [label "&Observed Heads"] [parent pest-menu] [callback dummy-callback])
    (new menu-item% [label "&Windows"] [parent pest-menu] [callback dummy-callback])
    (new separator-menu-item% [parent pest-menu])
    (new menu-item% [label "&Run"] [parent pest-menu] [callback dummy-callback])

    (define help-menu (new menu% (label "&About") (parent menu-bar)))
    (new menu-item% [label "&View License"] [parent help-menu] [callback dummy-callback])
    (new menu-item% [label "&Documentation"] [parent help-menu] [callback dummy-callback])
    (new menu-item% [label "&About"] [parent help-menu] [callback dummy-callback])
    
    ))

(define awfm:gui:mainwindow (new awfm:gui:mainwindow% [label "AWFM"]
                                 [height 400]
                                 [width 1200]))

(send awfm:gui:mainwindow show #t)