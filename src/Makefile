RACKET_SOURCES = aquiferdrawdown.rkt awfm-ffi.rkt dataframe.rkt mainwindow.rkt model.rkt \
	timeseries.rkt transientparameter.rkt units-dlg.rkt utility.rkt well.rkt wellloss.rkt

awfm.exe: $(RACKET_SOURCES)
	raco exe --gui mainwindow.rkt

clean:
	del /S *.bak
