(in-package :rsql)
; SQL Readtable for RSQL statement parsing

(set-macro-character #\' #'read-single-quote-string nil *sql-readtable*)
(set-macro-character #\, (lambda (stream char) (declare (ignore stream char)) (values)) nil *sql-readtable*)