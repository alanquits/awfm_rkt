#lang racket

(require db)
(require "model.rkt")

(define (delete-file-if-exists file-name)
  (if (file-exists? file-name)
      (delete-file file-name)
      '()))

(define (initialize-awfm-db-file file-name)
  (delete-file-if-exists file-name)
  (call-with-input-file "resources/create_db.sql"
    (lambda (in)
      (let ([conn (sqlite3-connect #:database file-name #:mode 'create)]
            [str (read-string 10000 in)])
        (let ([cmds (string-split str ";")])
         (map (lambda (cmd) (query-exec conn cmd)) cmds)
          (disconnect conn))))))

(define (save-model m file-name)
  (initialize-awfm-db-file file-name)
  (define conn (sqlite3-connect #:database file-name))

  (define (save-model-settings)
    (query-exec conn (string-append "update settings "
                                    "set length_unit = $1 "
                                    ",   time_unit = $2 "
                                    ",   discharge_unit = $3"
                                    ",   aquifer_drawdown_model = $4"
                                    ",   well_loss_turbulant_on = $5"
                                    ",   well_loss_laminar_on = $6"
                                    ",   well_loss_transient_on = $7"
                                    ",   h0_transient_on = $8")
                (send m get-unit "length")
                (send m get-unit "time")
                (send m get-unit "discharge")
                (send (get-field aquifer-drawdown-model m) name)
                (send m get-flag "well-loss-turbulant-on")
                (send m get-flag "well-loss-laminar-on")
                (send m get-flag "well-loss-transient-on")
                (send m get-flag "h0-transient-on")))
  (disconnect conn))
              
  

(define (import-model file-name)
  (define conn (sqlite3-connect file-name #:mode 'read-only))
  (define m (new model%))

  (begin
    (define settings-row (query-row conn (string-append
                     "select length_unit, time_unit, discharge_unit, "
                     "aquifer_drawdown_model, "
                     "well_loss_turbulant_on, well_loss_laminar_on, well_loss_transient_on, h0_transient_on")))

    (send* m
      (set-unit "length" (vector-ref settings-row 0))
      (set-unit "time" (vector-ref settings-row 1))
      (set-unit "discharge" (vector-ref settings-row 2))
      (set-flag "well-loss-turbulant-on" (vector-ref 3))
      (set-flag "well-loss-laminar-on" (vector-ref 4))
      (set-flag "well-loss-transient-on" (vector-ref 5))
      (set-flag "h0-transient-on" (vector-ref 6)))
    )
  m
  )

(initialize-awfm-db-file "awfm_initialized.db")
(define m (new model%))
(send* m
  (set-unit "length" "feet")
  (set-unit "time" "seconds"))
(save-model m "temp.db")
  