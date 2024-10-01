(in-package :rsql)
; Data-stream  is opened as an 8-bit stream
; Following function definitions are for reading 8-bit, 16-bit and 32-bit values from the stream
; Subsequently a function to read a sequence of chars as 8- and 16-bit from the stream

(defun read-data (stream data-type &optional data)
	(when (>= (file-position stream) (file-length stream))
		(return-from read-data nil))
	(case data-type	
		(-1 (read-64bit-big-endian stream))			; Index Integer (signed 64-bit big endian)
		(0  (read-8bit-value stream)) 				; Byte (8-bit)
		(1  (read-signed-8bit-value stream))		; Tinyinte (signed 8-bit)
		(2  (read-16bit-value stream)) 				; Small integer (signed 16-bit)
		(3  (read-signed-24bit-value stream)) 		; Medium integer (signed 24-bit)
		(4  (read-signed-32bit-value stream)) 		; Integer (signed 32-bit)
		(5  (read-32bit-value stream)) 	; Positive Only Integers(unsigned 32-bit)
		(6  (read-signed-64bit-value stream)) 		; Big Integers(signed 64-bit)
		(7  (read-utf-8-charseq stream 				; String (UTF-8)
				(read-32bit-value stream)))
		(8  (read-32-bit-float stream)) 			; IEEE 754 floating point 32-bit representation
		(9  (read-64-bit-float stream)) 			; IEEE 754 floating point 64-bit representation
		(10 (read-32-bit-decimal stream)) 			; Custom signed decimal 32-bit representation
		(11 (read-64-bit-decimal stream)) 			; Custom signed decimal 64-bit representation
		(12 (read-bcrypt-password stream data)) 	; Password hashed with bcrypt (fastest)
		(13 (read-scrypt-password stream data)) 	; Password hashed with scrypt
		(14 (read-argon2-password stream data)) 	; Password hashed with argon2 (slowest)
		(15 (read-date stream)) 					; Date
		(16 (read-datetime stream)) 				; Datetime
		(17 (read-timestamp stream)) 				; Timestamp (unsigned 40-bit integer)
		(18 (read-time stream)) 					; Time
		(19 (read-year stream)) 					; Year
	))
	
	
(defun skip-data (stream data-type)
	(when (>= (file-position stream) (file-length stream))
		(return-from skip-data nil))
		
	(case data-type	
		(-1 (file-position stream (+ 8	(file-position stream))))	; Index Integer (signed 64-bit big endian)
		(0  (file-position stream (+ 1	(file-position stream)))) 	; Byte (8-bit)
		(1  (file-position stream (+ 1	(file-position stream))))	; Tinyinte (signed 8-bit)
		(2  (file-position stream (+ 2	(file-position stream)))) 	; Small integer (signed 16-bit)
		(3  (file-position stream (+ 3	(file-position stream)))) 	; Medium integer (signed 24-bit)
		(4  (file-position stream (+ 4	(file-position stream)))) 	; Integer (signed 32-bit)
		(5  (file-position stream (+ 4	(file-position stream)))) 	; Positive Only Integers(unsigned 32-bit)
		(6  (file-position stream (+ 8	(file-position stream)))) 	; Big Integers(signed 64-bit)
		(7  (file-position stream (+	(read-32bit-value stream)  	; String (UTF-8)
										(file-position stream))))
		(8  (file-position stream (+ 4	(file-position stream)))) 	; IEEE 754 floating point 32-bit representation
		(9  (file-position stream (+ 8	(file-position stream)))) 	; IEEE 754 floating point 64-bit representation
		(10 (file-position stream (+ 4	(file-position stream)))) 	; Custom signed decimal 32-bit representation
		(11 (file-position stream (+ 8	(file-position stream)))) 	; Custom signed decimal 64-bit representation
		(12 (file-position stream (+ 40 (file-position stream)))) 	; Password hashed with bcrypt (fastest)
		(13 (file-position stream (+ 40 (file-position stream)))) 	; Password hashed with scrypt
		(14 (file-position stream (+ 80 (file-position stream)))) 	; Password hashed with argon2 (slowest)
		(15 (read-date stream)) 					; Date
		(16 (file-position stream (+ 6 (file-position stream))))	; Datetime
		(17 (file-position stream (+ 5 (file-position stream)))) 	; Timestamp (unsigned 40-bit integer)
		(18 (read-time stream)) 					; Time
		(19 (file-position stream (+ 2 (file-position stream))))	; Year
	))

(defun skip-multiple-data (stream datatypes)
	"Using SKIP-DATA skip a composition of multiple data-types"
	(when (< (length datatypes) 1)
		(return-from skip-multiple-data nil))
	(dolist (tp datatypes)
		(unless (skip-data stream tp)
			(return-from skip-multiple-data nil)))
	t)

; Read table overview
; Section 1: 1 byte containing the number of fields, and then field names as a string of 8-bit chars
; Field names are additionally prefaced with an integer containing an integer containing the length of the name
; Then it has the datatype enumeration for that field stored in 8 bits , and then an 8 bit representation of truthness of 
; 	- PRIMARY KEY
;	- UNIQUE 
;	- AUTO_INCREMENT
;	- NULL (NOT NULL :FALSE)

; Section 2: Number of indexes(1 byte) then field names as 8-bit chars, that is used as indexes. Default is id.
; These field-names are similarly prefaced with an integer containing the length of the name, but not the datatype

; STRUCTURE
; Num_fields(8-bit) field_length (8-bit) field_name (8-bit charseq)  field_datatype (8-bit) 
; Num_indexes (8-bit) field_length (8-bit) field_name (8-bit charseq)

(defun read-table-form (schema-name table-name)
	(let ((table-form (make-instance 'table))
		  (current-field) (field-name) (field-info))
		(with-open-file (stream (concatenate 'string *data-dir* (string schema-name) "/" (string table-name) ".tbl")
							:direction :input
							:element-type '(unsigned-byte 8))
							

			(loop for i from 0 below (read-byte stream) do 			 		; Iterate across the field names
				(setf current-field (make-instance 'field :rownum i))		; Create a FIELD instance
				(setf field-name 											; Read the FIELD name
					(read-from-string
						(read-8bit-charseq stream (read-byte stream))))
				(setf (name current-field) field-name)						; Set FIELD name
				(setf (datatype current-field) (read-byte stream))			; Retrieve the datatype enumeration
				(setf field-info (read-field-info stream))					; Retrieve the addition field-info
				(setf (primary    	  current-field) (aref field-info 0))	; Extract field-info
				(setf (unique	  	  current-field) (aref field-info 1))
				(setf (auto_increment current-field) (aref field-info 2))
				(setf (nul 			  current-field) (aref field-info 3))
				(setf (gethash field-name									; Insert the field into the table-form
						(fields table-form)) current-field)
				(when (eql (aref field-info 0) :TRUE)						; If field is a PRIMARY KEY add it to TABLES:PRIMARY
					(setf (gethash field-name
								(primary table-form)) 
									(rownum current-field))))						
			(loop for i from 0 below (read-byte stream) do					; Iterate across indexes
				(setf field-name 											; Read the FIELD name
					(read-from-string
						(read-8bit-charseq stream (read-byte stream))))
				(setf (gethash field-name 									; Insert field-name and rownum into TABLES:INDEXES
						(indexes table-form))
							(read-byte stream))))
		 table-form))


; Read the indexing file to determine the number of files and rows used for a specific table.
; The indexing file begins with an 8-bit integer representing the file number.
; Each entry includes:
;			- The field value
; 			- An 8-bit integer indicating the file number
; 			- A 32-bit integer representing the file position 
;			 (allowing files up to 4MB).

; READ-DATA-SIZE reads the file number and iterates through the file, skipping data based on the key-fields' data types
; and advancing 5 bytes per entry (1 byte for the file number and 4 bytes for the file position).

(defun read-data-size (schema-name table-name keys keytypes)
	"Returns a CONS of file number and ROW COUNT
	KEYS already carry the neccesary _ for filename"
	(when (< (length keytypes) 1)
		(return-from read-data-size (list 0 0 0)))
	(with-open-file 
		(stream (concatenate 'string 
							 *data-dir*
							 (string schema-name) "/" 
							 keys
							 (string table-name) ".idx")
						:direction :input
						:element-type '(unsigned-byte 8))
		(list 	(read-8bit-value stream)
				(loop	for i from 0
						for d = (skip-multiple-data stream keytypes)
						while d do
							(file-position stream (+ 5 (file-position stream)))
						finally (return i))
						(if (> (file-length stream) 8)					; If the file-length is larger than 8, expect a 32-bit integer as a file-position designator
							(progn
								(file-position stream ( - (file-position stream) 4))
								(read-32bit-value stream))
							0))))

; Read row reads a row from STREAM using datatypes in TABLE-FORM and inserts them into rset
; If entries in RSET are not :CLEAR those values will not be re-read							
(defun read-row (stream table-form rset)
	"Read a single row from file-stream"
	(loop for i from 0 below (length rset) do
		(if (eql (aref rset i) :CLEAR)
			(setf (aref rset i)
				(read-data stream (datatype
					(aref (fieldarr table-form) i))))
			(skip-data stream (datatype
						(aref (fieldarr table-form) i))))))
					 