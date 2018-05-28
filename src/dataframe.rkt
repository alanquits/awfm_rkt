#lang racket

(require csv-reading)
(require db)

(provide dataframe-sqlite%
         dataframe-csv%)

(define dataframe-interface<%>
  (interface () open close next-record get-value headers tables set-table))

; SQLITE Implementation
(define dataframe-sqlite%
  (class* object% (dataframe-interface<%>)

    (init-field file-path)

    (define conn null)
    
    (define results '())
    (define table-name "TABLE-NOT-SET")
    (define record '())

    (define/public (headers)
      (if (string=? table-name "TABLE-NOT-SET")
          #f
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
          #f
          (begin
            (set! record (car results))
            (set! results (cdr results))
            #t)))

    (define/public (open)
      (set! conn (sqlite3-connect #:database file-path))
      (if (connected? conn)
          '(#t "")
          '(#f "Unable to connect to SQLite3 database"))
      )

;    (define/public (spinup)
;      (if (string=? table-name "TABLE-NOT-SET")
;          (void)
;          (set! results (query-exec "select * from $1" table-name))))

    (define/public (set-table new-table-name)
      (set! table-name new-table-name))
    
    (define/public (tables)
      (query-list conn "SELECT name FROM sqlite_master WHERE type='table'"))

    (open)
    (super-new)))

; CSV Implementation
(define dataframe-csv%
  (class* object% (dataframe-interface<%>)

    (init-field file-path)
    (define results '())
    (define record '())
    (define next-row null)
    (define cached-headers '())
    
    (define csv-reader (make-csv-reader-maker
         '((separator-chars            #\,)
           (strip-leading-whitespace?  . #t)
           (strip-trailing-whitespace? . #t))))

    (define/public (next-record)
      (set! record (next-row))
      (if (null? record)
          #f
          #t))

    (define/public (open)
      (let ([f (open-input-file file-path)])
        (if (input-port? f)
            (begin
              (set! next-row (csv-reader f))
              (set! cached-headers (next-row))
              '(#t ""))
            '(#f (format "Unable to open ~a for reading." file-path)))))

    (define/public (close)
      (void))

    (define/public (get-value idx)
      (list-ref record idx))

    (define/public (headers)
      cached-headers)

    (define/public (tables)
      '())

    (define/public (set-table new-table-name)
      (void))
        
))
      
