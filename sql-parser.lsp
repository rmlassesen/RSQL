(in-package :rsql)
; SQL Parser

(defun sql-parser (stream)
	(let ((*readtable* *sql-readtable*))
		(loop for param = (read stream nil nil)
			while param do
				(case param
					('use (use-db stream))
					('create (case (read stream nil nil)
								('table (create-table stream))
								('database (create-database stream))
								(otherwise (error "Unknown CREATE parameter"))))
					('insert (if (eql (read stream nil nil) 'into)
									(insert-into stream)
									(error "Unknown INSERT parameter")))
					('select (select-from stream))
					(otherwise (error "Un-recognized parameter ~a" param))))))