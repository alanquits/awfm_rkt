#lang racket/gui

(require "units-dlg.rkt")

(struct awfm-gui-state (model is-dirty))


(define (dummy-callback button event)
  '())

(define (quit button event)
  (exit))

(define frame (new frame% [label "AWFM"]
                   [height 400]
                   [width 1200]))

;; Menu bar
(define menu-bar (new menu-bar%
                      (parent frame)))

; File menu
(define file-menu (new menu% (label "&File") (parent menu-bar)))
(new menu-item% [label "&New"] [parent file-menu] [callback dummy-callback])
(new menu-item% [label "&Open"] [parent file-menu] [callback dummy-callback])
(new separator-menu-item% [parent file-menu])
(new menu-item% [label "&Save"] [parent file-menu] [callback dummy-callback])
(new menu-item% [label "Save &As"] [parent file-menu] [callback dummy-callback])
(new separator-menu-item% [parent file-menu])
(new menu-item% [label "&Quit"] [parent file-menu] [callback quit])

; Model menu
(define model-menu (new menu% (label "&Model") (parent menu-bar)))
(new menu-item% [label "&Units"] [parent model-menu] [callback dummy-callback])
(new menu-item% [label "&Wells"] [parent model-menu] [callback dummy-callback])
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

(send frame show #t)