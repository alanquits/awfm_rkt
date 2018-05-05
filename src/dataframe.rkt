#lang racket

(require db)

(define dataframe-interface<%>
  (interface () open close spinup next-record get-value headers tables set-table))

; SQLITE Implementation
(define dataframe-sqlite%
  (class* object% (dataframe-interface<%>)

    (init-field
     file-path)

    (define conn (sqlite3-connect #:database file-path))

    (define results '())
    (define table-name "TABLE-NOT-SET")
    (define record '())

    (define/public (headers)
      (if (string=? table-name "TABLE-NOT-SET")
          '()
          (let ([results (query conn (string-append "PRAGMA table_info(" table-name ")"))])
            (map (lambda (x) (vector-ref x 1)) (rows-result-rows results)))))


    (define/public (headers-index hdr)
      (vector-memv hdr (list->vector (headers))))
    
    (define/public (close)
      (display "sqlite:close called\n")
      '())

    (define/public (get-value idx)
      (vector-ref record idx))
    
    (define/public (next-record)
      (if (null? results)
          '()
          (begin
            (set! record (car results))
            (set! results (cdr results))
            '())))

    (define/public (open)
      '())

    (define/public (spinup)
      '())

    (define/public (set-table new-table-name)
      (set! table-name new-table-name))
    
    (define/public (tables)
      (query-list conn "SELECT name FROM sqlite_master WHERE type='table'"))

    (super-new)))

(define df (make-object dataframe-sqlite% "test.db"))
(send df tables)
(send df headers)
(send df set-table "locations")
(send df headers)