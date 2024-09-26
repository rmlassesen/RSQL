(in-package :rsql)
; SQL Select Handling

(defun make-select-form (stream)
	"Extract clauses and table-information from stream"
	(let* ( (*readtable* *SQL-readtable*)
			(rows (make-array 0 :element-type 'cons
								:fill-pointer 0
								:adjustable t))
			(from-table)
			(clause))
		(loop for obj = (read stream nil nil)
			while obj do
				(cond 
					((eql obj 'from)  (setf from-table (read stream)))
					((eql obj 'where) (setf clause (read-clause stream)))
					((eql obj 'as) (rplacd 
									(aref rows (- (length rows) 1)) 
									(read stream)))
					(t (vector-push-extend (cons obj :nil) rows))))
					
		(list from-table rows clause)
		
		))
		

(defun select-from (stream)
	"Select data from database"
	(let* ( (select-form (make-select-form stream))
			(table-form  (read-table-form (first select-form))))
			
			))
	
	


(defun test-s ()
	(select-from (make-string-input-stream 
		"* FROM MOCK_DATA WHERE ID = 1;")))
