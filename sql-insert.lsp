(in-package :rsql)
; SQL Insert Handling


(defun make-insert-form (stream)
	"Extract insert data from stream"
	(let*  ((*readtable* *SQL-readtable*)
			(dummy (set-macro-character #\, (lambda (stream char) (declare (ignore stream char)) (values)))) ; Read commas as NOTHING instead of :NEXT
			(table-name (read stream nil nil))
			(table-fields (fields (gethash table-name 
				(tables (gethash *in-db* *schemas*)))))
			(valuelist (read stream nil nil))
			(fieldnums)
			(sets	 (make-array 0 :fill-pointer 0 :element-type t :adjustable t))			
			(fields	 (make-array (hash-table-count table-fields) :element-type t))
			(fvalues (make-array (hash-table-count table-fields) :element-type t :initial-element 'null)))
		(declare (ignore dummy))	
		(maphash (lambda (k v)
			(declare (ignore k))
			(setf (aref fields (rownum v)) v))									; Assign FIELD NAME to array matching the fields ROW NUMBER
			table-fields)
			
		(cond 
			((not table-fields)	(error "INSERT statement is malformed - missing correct table name"))
			((not valuelist)	(error "INSERT statement is malformed - missing VALUES"))
			((eql valuelist 'VALUES)
				(loop for valuelist = (read stream nil nil)						; Retrieve (multiple) valuelist(s)
					while (and valuelist (listp valuelist)) do
						(unless (= (length valuelist) (length fields))			; Check for the correct number of values in VALUES
							(error "Number of values do not correspond number of fields: ~a" (length valuelist)))
						(loop for val in valuelist								; Loop through VALUELIST
							  for i from 0 do
								(if (eql val 'null)
								  (progn
									(if (eql (nul (aref fields i)) :FALSE) 		; Check if NULL value is allowed to be NULL
										(error "~a cannot be NULL" (name (aref fields i)))
										(setf (aref fvalues i) 
											(gethash (datatype (aref fields i)) *null-values*)))
									(when (eql (aref fvalues i) 'error)
										(error "Datatype cannot be NULL: ~a" 
											(gethash (datatype (aref fields i)) *datatypes-enum*))))
									(if (checktype val (datatype (aref fields i)))
										(setf (aref fvalues i) val)
										(error "~a does not match its designated datatype: ~a" val (datatype (aref fields i))))))
						(vector-push-extend (copy-seq fvalues) sets)))			; Push the values into the SETS array
		((listp valuelist)
			(setf fieldnums (make-array (length valuelist) :element-type 'integer))
			(loop for fld in valuelist 											; Loop across provided fields and update 'null to 'reserved to corresponding values in FVALUES
				  for i from 0 do
					(setf (aref fvalues (rownum (gethash fld table-fields)))
						'reserved)
					(setf (aref fieldnums i) (rownum (gethash fld table-fields))))
			(loop for i from 0 below (length fvalues) do
				(when (eql (aref fvalues i) 'null)								; Loop across fvalues and set value to corresponding NULL values for the datatype
					(when (and  (eql (nul 			 (aref fields i)) :FALSE)
								(eql (auto_increment (aref fields i)) :FALSE))
						(error "~a cannot be NULL" (name (aref fields i)))) 
					(setf (aref fvalues i)
						(gethash (datatype (aref fields i)) *null-values*))))
			(unless (eql (read stream nil nil) 'VALUES)
				(error "Expected VALUES keyword")) 
			(loop for valuelist = (read stream nil nil)							; Retrieve (multiple) valuelist(s
				while (and valuelist (listp valuelist)) do
				(unless (= (length valuelist) (length fieldnums))				; Check for the correct number of values in VALUES
					(error "Number of values do not correspond number of fields: ~a" (length valuelist)))	
				(loop for val in valuelist										; Loop through VALUELIST
				  for i from 0 do
					(if (eql val 'null)
					  (progn
						(if (eql (nul (aref fields (aref fieldnums i))) :FALSE) 	; Check if NULL value is allowed to be NULL
							(error "~a cannot be NULL" (name (aref fields (aref fieldnums i))))
							(setf (aref fvalues (aref fieldnums i)) 
								(gethash (datatype (aref fields (aref fieldnums i))) *null-values*)))
						(when (eql (aref fvalues (aref fieldnums i)) 'error)
							(error "Datatype cannot be NULL: ~a" 
								(gethash (datatype (aref fields (aref fieldnums i))) *datatypes-enum*))))
						(if (checktype val (datatype (aref fields (aref fieldnums i))))
							(setf (aref fvalues (aref fieldnums i)) val)
							(error "~a does not match its designated datatype: ~a" val (datatype (aref fields (aref fieldnums i)))))))
						(vector-push-extend (copy-seq fvalues) sets)))			; Push the values into the SETS array							
		(t (error "INSERT statement is malformed near: ~a" valuelist)))
		(when (< (length sets) 1)
			(error "No values provided"))
		(cons fields sets)))

(defun insert-into (stream)
	"Insert data into database"
	(let*  ((insert-form (make-insert-form stream)))					; Redirect STREAM to INSERT parser
		insert-form))

(defun test-i ()
	(insert-into (make-string-input-stream 
		"MOCK_DATA (id, first_name, last_name, email, gender, ip_address) values (1, 'Tybie', 'Brownstein', 'tbrown\\'stein0@wufoo.com', 'Female', '54.77.166.145');")))	