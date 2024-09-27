(in-package :rsql)
; SQL Table Handling

(defun make-table-form (stream)
	"From a CREATE TABLE command make a table form"					; Create string takes the form of everything after "CREATE TABLE" until ;
	(let*  ((*readtable* *sql-readtable*) 
			(current-field :NIL) 
			(field-num 0) (key)
			(table-name (read stream))
			(rows (read stream))
			(tbl (make-instance 'table)))
		(setf (name tbl) table-name)	
		(unless (listp rows)
			(error "CREATE TABLE is malformed - expected ("))
		(setf rows (list-to-array rows))
		(loop for i from 0 below (length rows) do
			(cond 
				((eql (aref rows i) 'NOT) 						; When encountering NOT expect NULL, otherwise ERROR
					(when (not (eql (aref rows (+ 1 i)) 'NULL))	
						(error "Unexpected parameter: NOT"))
					(setf (nul (gethash current-field (fields tbl))) :FALSE) 
					(incf i)) 									; Set NUL of CURRENT-FIELD to :FALSE and increment i
				((eql (aref rows i) 'AUTO_INCREMENT)			; When encountering AUTO_INCREMENT set auto_increment of CURRENT-FIELD to :TRUE
					(setf ( auto_increment (gethash current-field (fields tbl))) :TRUE))
				((listp (aref rows i)) )						; If object is a list, handle it as ARGUMENTS of the previous object
				((gethash (aref rows i) *datatypes*)			; Check if symbol is a datatype - if so add datatype enumeration to number of CURRENT-FIELD
						(setf (datatype (gethash current-field (fields tbl))) 
						(gethash (aref rows i) *datatypes*)))
				((eql (aref rows i) 'PRIMARY)					; When encountering PRIMARY expect KEY, otherwise ERROR
					(when (not (eql (aref rows (+ 1 i)) 'KEY))
						(error "Unexpected parameter: PRIMARY"))
					(setf i (+ i 2))							; Increase I by 2 to reach argument/key list
					(dolist (k (aref rows i))					; Iterate through key-list	
						(unless (setf key (gethash k (fields tbl)))
							(error "Provided PRIMARY KEY ~a is not a field" k))
						(setf (primary key) :TRUE)
						(setf (gethash k (primary tbl)) (rownum key))))
				((eql (aref rows i) :NEXT) 						; Set CURRENT-FIELD to :NIL
						(setf current-field :NIL))
				(t 	(when (not (eql current-field :NIL))		; Make sure that CURRENT-FIELD is :NIL otherwise error
						(error "Unexpected field designation: ~a" (aref rows i)))
					(setf current-field (aref rows i))			; Set CURRENT-FIELD as the read object
					(when (gethash current-field (fields tbl)) 	; ERROR if FIELD NAME is already assigned to the table
						(error "Duplicate of field name ~a" current-field))
					(setf (gethash current-field (fields tbl))	; Make a new field instance in the table's FIELD hash-table and set ROWNUM to FIELD-NUM
						(make-instance 'field :rownum field-num))
					(incf field-num))))							; Increment FIELD-NUM
	tbl))				

(defun create-table (stream)
	"Create a table-form from STREAM, write it to files, and store it in the schema"
	(let ((table-form (make-table-form stream)))
		(when (gethash *in-db* *schemas*)
			(when (gethash (name table-form)		; Check if the table already exist if so ERROR
				(tables (gethash *in-db* *schemas*)))
					(error "Table ~a already exist!" (name table-form))))
		(write-table-form table-form)				; Write table-form data to files
		(setf (gethash (name table-form)			; Insert table form in hash-table with key as 'table-name
			(tables (gethash *in-db* *schemas*)))	; into SCHEMA of current database
			table-form)))

(defun test-t ()
	(with-input-from-string (table-stream
				"create table mock_data (id int(2) NOT NULL AUTO_INCREMENT, first_name varchar(25) NOT NULL, last_name varchar(25) NOT NULL, email varchar(25) NOT NULL, gender varchar(25) NOT NULL, ip_address varchar(25) NOT NULL, PRIMARY KEY(id)) ENGINE=InnoDB;")
	(sql-parser table-stream)))
