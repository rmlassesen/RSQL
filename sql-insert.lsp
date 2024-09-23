(in-package :rsql)
; SQL Insert Handling

(defun make-insert-form (stream)
	"Extract insert data from stream"
	(let ((*readtable* *SQL-readtable*) (table-name) (vallen) (rows)
		(vals (make-array 0 :element-type t :fill-pointer 0 :adjustable t)))
		(setf table-name (read stream))
		(setf rows (read stream))
		(if (eql rows 'VALUES)
			(vector-push-extend (read stream) vals)
			(read stream))
		(loop for val = (read stream nil nil)
			while val do
				(if (listp val)
					(vector-push-extend (list-to-array val) vals)
					(return)))
		(setf vallen (length (aref vals 0)))
		(if (listp rows)
			(progn
				(loop for v across vals do
					(when (not (= (length rows) (length v)))
						(error "The number of ROWS and VALUES do not match")))
				(setf rows (list-to-array (mapcar #'from-keyword rows))))
			(progn 
				(loop for v across vals do
					(when (not (= vallen (length v)))
						(error "The number VALUES are of different length"))
						(setf vallen (length v)))
				(setf rows #((from-keyword rows)))))
				
		(list 	(from-keyword table-name) rows vals)))

(defun insert-into (stream)
	"Insert data into database"
	(let*  ((insert-form (make-insert-form stream))					; Redirect STREAM to INSERT parser
			(vals (make-array (length (aref (third insert-form) 0))))
			(table-form (read-table-form (first insert-form))))		; Retrieve table-form from table-name in INSERT-FORM
		(when (> (length (aref (third insert-form) 0))				; Check if the number of VALUES are more than the number of ROWS in the TABLE
				 (length (second table-form)))
					(error "Too many VALUES in INSERT statement"))
		(when (= 1 (length (second insert-form)))					; If VALUES are added directly without row specification
			(when (not (= (length (aref (third insert-form) 0))		; Make sure that the number of VALUES match the ROWS of the TABLE
						  (length (second table-form))))
							(error "Number of VALUES does not match number of ROWS in TABLE ~a" (first insert-form))))
		; TODO Make sure that VALUES are in the correct order
		; TODO check if non asigned VALUES can be NULL otherwise ERROR
		(loop for i below (length (second table-form)) do
			(setf (aref (second table-form) i) (cdr (aref (second table-form) i))))
		(write-rows (first table-form) (second table-form) (third insert-form) (third table-form))))

(defun test-i ()
	(insert-into (make-string-input-stream 
		"MOCK_DATA (id, first_name, last_name, email, gender, ip_address) values (1, 'Tybie', 'Brownstein', 'tbrown\\'stein0@wufoo.com', 'Female', '54.77.166.145');")))	