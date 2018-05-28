#lang racket/gui

(require framework/gui-utils)
(require "dataframe.rkt")

(define dataframe-dlg%
  (class dialog%
    
    (init-field parent
                targets)

    (field [dataframe null])
    (field [accepted null])
    
    (super-new [parent parent]
               [label "Import Dataframe"])

    (define (accept-callback b e)
      (set! accepted #t)
      (send parent show #f))

    (define (cancel-callback b e)
      (set! accepted #f)
      (if (null? dataframe)
          (void)
          (send dataframe close))
      (send parent show #f))

    (define (load-csv)
      (define csv-settings-dlg (new dialog% [parent this]
                                    [label "CSV settings"]))
      (define panel (new horizontal-panel% [parent csv-settings-dlg]))
      (define delimiter-choice (new choice%
                    (label "Delimiter:  ")
                    (parent csv-settings-dlg)
                    (choices (list "," ";" "|" "TAB"))))
      (send csv-settings-dlg show #t))

    (define (unload-dataframe)
      (send* table-choices
        [clear]
        [enable #f])
      (set! dataframe null))
        

    (define (load-dataframe)
      (let* ([file-name (send file-name-text-field get-value)]
             [ext (string-downcase (format "~a" (path-get-extension file-name)))])
        (cond [(string=? ext ".csv") (load-csv)]
              [(string=? ext ".db") (load-sqlite file-name)]
              [else (message-box "Unrecognized file type"
                                 (string-append
                                  "File type not supported. Supported file types are:\n"
                                  "  - SQLite database (.db) \n"
                                  "  - Comma-delimited text/ASCII file (.csv)\n")
                                  this)])))

    (define (load-sqlite file-name)
      (let* ([df (new dataframe-sqlite% [file-path file-name])]
             [open-status (send df open)])
        (if (car open-status)
            (begin
              (send table-choices clear)
              (map (lambda (tbl) (send table-choices append tbl))
                   (send df tables))
              (send table-choices enable #t)
              (set! dataframe df))
            (message-box "Problem loading dataframe"
                         (cadr open-status)
                         this))))
            
        

    (define (file-load-button-callback b e)
      (load-dataframe))
    
    (define (browse-button-callback b e)
      (let ([file-name (get-file "Open dataframe" this #f #f #f null '(("Dataframe" "*.*")))])
        (send file-name-text-field set-value (path->string file-name))
        (toggle-load-button)
        (displayln file-name)))

    (define (file-name-text-field-callback b e)
      (if (symbol=? (send e get-event-type) 'text-field)
          (begin
            (toggle-load-button)
            (if (null? dataframe)
                (void)
                (unload-dataframe))
            )
          (displayln "a different event")))

   (define (table-choice-callback b e)
     (let* ([table-name (send table-choices get-string-selection)]
            [headers (if (null? dataframe) #f (send dataframe headers))])
       (
        (displayln (send table-choices get-string-selection))
        (displayln b)
        (displayln e))))

    (define (toggle-load-button)
      (if (file-exists? (send file-name-text-field get-value))
          (send file-load-button enable #t)
          (send file-load-button enable #f)))

    (define main-layout (new vertical-panel% [parent parent]
                             [vert-margin 20]
                             [horiz-margin 20]))

    (define file-loader-box (new horizontal-panel% [parent main-layout]))
    (define file-name-text-field (new text-field%
                                      (label "File:   ")
                                      (parent file-loader-box)
                                      [min-width 200]
                                      [callback file-name-text-field-callback]))

    (define file-browse-button (new button%
                                    [parent file-loader-box]
                                    [label "Browse"]
                                    [callback browse-button-callback]))

    (define file-load-button (new button%
                                  [parent file-loader-box]
                                  [label "Load"]
                                  [enabled #f]
                                  [callback file-load-button-callback]))


    (define table-choices (new choice% [parent main-layout]
                               [label "Table:  "]
                               [choices '()]
                               [enabled #f]
                               [min-width 300]
                               [vert-margin 20]
                               [callback table-choice-callback]))


    (define targets-group-box (new group-box-panel%
                             (parent main-layout)
                             (label "Targets")))

    (define targets-choices-hash (make-hash))
    (map (lambda (t) (hash-set! targets-choices-hash t
                                (new choice%
                                     [parent targets-group-box]
                                     [label (string-append t ":  ")]
                                     [choices '()]
                                     [enabled #f])))
         targets)

    (define button-box (new horizontal-panel% [parent main-layout]))
    (gui-utils:ok/cancel-buttons button-box accept-callback cancel-callback "Ok" "Cancel")                                 
    
    ))

(define frame (new frame% [label "TEST"]))
(define dlg (new dataframe-dlg%
                 [parent frame]
                 [targets '("WellID" "X" "Y")]))
(send frame show #t)

