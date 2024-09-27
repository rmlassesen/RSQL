(in-package :rsql)
; SQL Parser

(defun sql-parse (stream)
	(let ((*readtable* *sql-readtable*))
		(loop for param = (read stream nil nil)
			while param do
				(case param
					(exit (return :EXIT))
					(use (use-db stream))
					(create (case (read stream nil nil)
								(table (create-table stream))
								(database (create-database stream))
								(otherwise (error "Unknown CREATE parameter"))))
					(insert (if (eql (read stream nil nil) 'into)
									(insert-into stream)
									(error "Unknown INSERT parameter")))
					(select (select-from stream))
					(engine (format t "I really don't care what database engine you want")
							(read stream nil nil)(read stream nil nil))
					(otherwise (error "Un-recognized parameter: ~a" param))))))
					
			
(defun sql-parser ()
"Scans SQL input to be parsed"
	(let ((ret))
	(loop while (not (eql ret :EXIT)) do
		(format t "~& RSQL > ")(force-output) ; Clears output stream(previous FORMAT call), before requesting input
		(handler-case (setf ret (sql-parse (make-string-input-stream (read-line))))
		(error (c) 
			(format t " ERROR: ~a" c)))))
		(format t "~% Goodbye! ~% R#UNE Readily Uniform Network Element Structured Query Lisp~%"))