(in-package :rsql)
; SQL Table Handling

(defun make-table-form (create-string)
	"From a CREATE TABLE command make a table form"					; Create string takes the form of everything after "CREATE TABLE" until ;
	(let* (	(table-name) (rows '()) (indexes '())					; Create a row and and indexing list
			(npos (search "(" create-string))						; Search ( to determine when the row specifications occur
			(row-data (split-string create-string "," (+ 1 npos))))	; Create an array of individual row specifications by splitting on ,
		(if (eql (aref create-string (- npos 1)) #\Space)			; Determine if the table name is ended by a space
			(setf table-name (subseq create-string 0 (position #\Space create-string)))
			(setf table-name (subseq create-string 0 npos)))
		(loop for e across row-data do
			(if (setf npos (search "primary key" e :test 'equalp))
				(progn
					(let ((idx (subseq e (+ 12 npos) (position #\) e :start npos))))
						(if (some (lambda (r) (equalp (car r) idx)) rows)
							(push idx indexes)
							(error "~a is NOT a specified row" idx))))
				(progn
					(setf npos (position-if #'alphanumericp e))
					(push 
						(cons 	(subseq e npos (setf npos (search " " e :start2 npos)))
								(gethash
									(subseq e (+ 1 npos) (setf npos (position-if-not #'alphanumericp e :start (+ 1 npos))))						
									*datatypes*))
						rows))))
		(list table-name (reverse rows) (reverse indexes))))

(defun test-t ()
	(let (( table-form (make-table-form "mock_data (id int(2) NOT NULL AUTO_INCREMENT, first_name varchar(25) NOT NULL, last_name varchar(25) NOT NULL, email varchar(25) NOT NULL, gender varchar(25) NOT NULL, ip_address varchar(25) NOT NULL, PRIMARY KEY(id)) ENGINE=InnoDB;")))
		(write-table-form 	(first 	table-form)
							(second table-form)
							(third 	table-form))))
