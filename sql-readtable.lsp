(in-package :rsql)
; SQL Readtable for RSQL statement parsing

(defun read-operator (stream char)
	(case char
		(#\< (if (eql (peek-char nil stream nil nil) #\=)
				(progn (read-char stream) '<=)
				'<))
		(#\> (if (eql (peek-char nil stream nil nil) #\=)
				(progn (read-char stream) '>=)
				'>))
		(#\! (if (eql (peek-char nil stream nil nil) #\=)
				(progn (read-char stream) '!=)
				(error "Unexpected operator '!' (expected !=)")))
		(otherwise  (intern (string char)))))

(set-macro-character #\' #'read-single-quote-string nil *sql-readtable*)
(set-macro-character #\, (lambda (stream char) (declare (ignore stream char)) :NEXT) nil *sql-readtable*)

; Call READ-OPERATOR on all clause operators
(set-macro-character #\< #'read-operator nil *sql-readtable*)
(set-macro-character #\> #'read-operator nil *sql-readtable*)
(set-macro-character #\= #'read-operator nil *sql-readtable*)
(set-macro-character #\+ #'read-operator nil *sql-readtable*)
(set-macro-character #\- #'read-operator nil *sql-readtable*)
(set-macro-character #\! #'read-operator nil *sql-readtable*)