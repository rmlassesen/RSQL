(in-package :rsql)
; Data-types: 
; 0		Index-integer (64-bit unsigned big endian integer) 
; 1		Bytes(unsigned 8-bit)
; 2		Integers(signed 64-bit)
; 3		Positive Only Integers(unsigned 64-bit)
; 4		Char-sequences/String(UTF-8)
; 5		IEEE 754 floating point 32-bit representation
; 6		IEEE 754 floating point 64-bit representation
; 7		Custom signed decimal 32-bit representation
; 8		Custom signed decimal 64-bit representation
; 9		Password hashed with bcrypt
; 10	Password hashed with scrypt
; 11	Password hashed with argon2i
; 12	Date
; 13	Datetime
; 14	Timestamp
; 15	Time
; 16	Year

; Write table overview
; Section 1: 1 byte containing the number of rows, and then row names as a string of 8-bit chars
; Row names are additionally prefaced with an integer containing an integer containing the length of the name
; And then ends with the datatype enumeration for that row
; Section 2: Number of indexes(1 byte) then row names as 8-bit chars, that is used as indexes. Default is id.
; These row-names are similarly prefaced with an integer containing the length of the name, but not the datatype

; STRUCTURE
; Section_End_Pos(16-bit) Num_rows(8-bit) Row_length (8-bit) Row_name (8-bit charseq) Row_datatype (8-bit) 
; Num_indexes (8-bit) Row_length (8-bit) Row_name (8-bit charseq)

; Arguments for table form "Table name as String" (list of (cons with row name and data-type)) (list of indexes)


(declaim (ftype (function (string list list)) write-table-form))

(defun write-table-form (table-name rows indexes)
	"Write .tbl file with information about the specific table"
	(with-open-file (stream (concatenate 'string *data-dir* table-name ".tbl")
						:direction :output
						:if-exists :supersede
						:element-type '(unsigned-byte 8))
		(write-8bit-value stream (length rows))					; Write the number of rows
		(dolist (r rows)										; Iterate over the rows-list
			(write-8bit-value stream (length (car r)))			; Write the length of the row-name
			(write-8bit-charseq stream (car r))					; Write the row-name as simple 8-bit chars
			(write-8bit-value stream (cdr r)))					; Write the datatype (integer)
		(write-8bit-value stream (length indexes))				; Write the number of Index Rows
		(dolist (r indexes)										; Iterate over Index Rows
			(write-8bit-value stream (length r))				; Write the length of the row-name
			(write-8bit-charseq stream r)						; Write the row-name as simple 8-bit chars
			(with-open-file (stream (concatenate 'string *data-dir* r "_" table-name ".idx")
							:direction :output
							:if-exists :supersede
							:element-type '(unsigned-byte 8))
							(write-64bit-value stream 0)))))	; Create an indexing file, and preface it with 0, as a data-file-size designator		


; Insert a row / write row
; Read the table overview, to accumulate the structure
; Open index file of table, for writing file-positions of rows in data files
; Open data files of table, for writing rows
; Index files contain sections representing each row, starting with the indexed row name (e.g. ID(default))
; A sections is formated with first a 64-bit representation of the ID as big endiannes, to optimize searching in the lower range, 
; subsequently not loosing significant effiecency in higher ranges.
; For each part of data written in a row section, the file-position is returned and written in the index-file.

(defun data-file-number (data-size)
	(if (= data-size 0)
		1 
		(+ 1 (truncate data-size 1024))))

(defun write-data (stream data data-type)
	(case data-type	
		(0 (write-64bit-big-endian stream data))	; Index-integer (unsigned 64-bit big endian)
		(1 (write-8bit-value stream data)) 			; Byte (8-bit)
		(2 (write-64bit-value stream data)) 		; Signed Integer (64-bit)
		(3 (write-signed-64bit-value stream data)) 	; Unsigned Integer (64-bit)
		(4 (write-32bit-value stream (length data)) ; String (UTF-8)
		   (write-utf-8-charseq stream data)) 		
		(5 (write-32-bit-float stream data)) 		; IEEE 754 floating point 32-bit representation
		(6 (write-64-bit-float stream data)) 		; IEEE 754 floating point 64-bit representation
		(7 (write-32-bit-decimal stream data)) 		; Custom signed decimal 32-bit representation
		(8 (write-64-bit-decimal stream data)) 		; Custom signed decimal 64-bit representation
		(9 (write-bcrypt-password stream data)) 	; Password hashed with bcrypt (fastest)
		(10 (write-scrypt-password stream data)) 	; Password hashed with scrypt
		(11 (write-argon2-password stream data)) 	; Password hashed with argon2 (slowest)
	)
	(file-position stream))

(defun write-rows (table-name data-types insert-values indexes)
	"Write a rows to the database file of table TABLE-NAME"
	(let (( idx-streams (make-array (length indexes))))
		(loop for i from 0 below (length indexes) do				; Open a stream for each PRIMARY KEY of the table
			(setf (aref idx-streams i) 
				(open (concatenate 'string 	
						*data-dir* 
						(aref indexes i)
						"_" table-name ".idx")
						:direction :io
						:if-exists :overwrite
						:element-type '(unsigned-byte 8))))

		(with-open-file (wstream  									; Open write-stream for data-file
							(concatenate 'string
								*data-dir*
								(princ-to-string
									(data-file-number 
										(read-64bit-value (aref idx-streams 0))))
								"_" table-name ".dat")
							:direction :output
							:if-does-not-exist :create
							:if-exists :append
							:element-type '(unsigned-byte 8))
					
		(loop for stream across idx-streams do						; Skip the first 64-bit of every index-streams
			(file-position stream 8))
		(loop for row across insert-values do						; Loop through every value-set
			(loop for stream across idx-streams do					; TEMPORARY write the line-start-position in all index-files 
				(write-32bit-value stream (file-position wstream)))
			(loop for i from 0 below (length row) do				; Loop through each entry in value-set
				(write-data wstream 								; Write corresponding data
							(aref row i)
							(aref data-types i))))
		(loop for stream across idx-streams do						; Write the file-length at the begininng of all index-files (Needs modification to include all file's sizes in combination)
				(write-32bit-value stream (file-position wstream))
				(when stream (close stream))))))					; Close the streams

