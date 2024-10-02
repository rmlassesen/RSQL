(in-package :rsql)
; Write table overview
; Section 1: 1 byte containing the number of fields, and then field names as a string of 8-bit chars
; Field names are additionally prefaced with an integer containing an integer containing the length of the name
; Then it has the datatype enumeration for that field, and then an 8 bit representation of truthness of 
; 	- PRIMARY KEY
;	- UNIQUE 
;	- AUTO_INCREMENT
;	- NULL (NOT NULL :FALSE)

; Section 2: Number of indexes(1 byte) then field names as 8-bit chars, that is used as indexes. Default is id.
; These field-names are similarly prefaced with an integer containing the length of the name, but not the datatype

; STRUCTURE
; Num_fields(8-bit) field_length (8-bit) field_name (8-bit charseq)  field_datatype (8-bit) 
; Num_indexes (8-bit) field_length (8-bit) field_name (8-bit charseq)

; Arguments for table form "Table name as String" (list of (cons with field name and data-type)) (list of indexes)

(defun write-table-form (table-form)
	"Write .tbl file with information about the specific table"
	(let ((pairs "") (table-name (string-downcase (string (name table-form)))))
		(with-open-file (stream (concatenate 'string 
											 *data-dir*
											 (string *in-db*) "/"
											 table-name 
											 ".tbl")
							:direction :output
							:if-exists :supersede
							:element-type '(unsigned-byte 8))
			(write-8bit-value stream 							; Write the number of fields
				(hash-table-count (fields table-form)))
			(maphash (lambda (k v) 								; Iterate across the fields of the table
				(setf k (string k))
				(write-8bit-value stream (length k))			; Write the length of the field-name
				(write-8bit-charseq stream k)					; Write the field-name as simple 8-bit chars
				(write-8bit-value stream (datatype v))			; Write the datatype as an 8-bit integer
				(write-field-info stream						; Write additional info in an 8-bit integer
					(primary v)
					(unique v)
					(auto_increment v)
					(nul v)))
				(fields table-form))
				(write-8bit-value stream 						; Write the number of PRIMARY KEY fields
				(hash-table-count (primary table-form)))		; (Preferably only 1, but more if the PRIMARY KEY is made up of multiple columns)
				(maphash (lambda (k v)
					(setf k (string k))
					(setf pairs 								; Construct PRIMARY KEY index file-name
						(concatenate 'string pairs k "_"))
					(write-8bit-value stream (length k))		; Write the length of the field-name
					(write-8bit-charseq stream k)				; Write the field-name as simple 8-bit chars
					(write-8bit-value stream v))				; Write ROWNUM of FIELD
					(primary table-form))						; Create an indexing file for the PRIMARY KEY
				(with-open-file (wstream (concatenate 'string *data-dir* (string *in-db*) "/" pairs table-name ".idx")
					:direction :output
					:if-exists :supersede
					:element-type '(unsigned-byte 8))
					(write-8bit-value wstream 1))				; Write 1 to the file for "Number of files"
				
			(write-8bit-value stream 							; Write the number of INDEXE fields
				(hash-table-count (indexes table-form)))			
			(maphash (lambda (k v)								; Write each ROWNUM
				(write-8bit-value stream v)						; Create an indexing file for each index
				(with-open-file (wstream (concatenate 'string *data-dir* (string *in-db*) "/" k "_" table-name ".idx")
									:direction :output
									:if-exists :supersede)))
				(indexes table-form))
			(with-open-file (wstream (concatenate 'string *data-dir* (string *in-db*) "/tables.rtb")
								:direction :output
								:if-exists :append
								:if-does-not-exist :create)
				(write-line table-name wstream)))))				; Write table name to tables.rtb


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
		(-1 (write-64bit-big-endian stream data))		; Index Integer (signed 64-bit big endian)
		(0  (write-8bit-value stream data)) 			; Byte (8-bit)
		(1  (write-signed-8bit-value stream data))		; Tinyinte (signed 8-bit)
		(2  (write-16bit-value stream data)) 			; Small integer (signed 16-bit)
		(3  (write-signed-24bit-value stream data)) 	; Medium integer (signed 24-bit)
		(4  (write-signed-32bit-value stream data)) 	; Integer (signed 32-bit)
		(5  (write-32bit-value stream data)) 			; Positive Only Integers(unsigned 32-bit)
		(6  (write-signed-64bit-value stream data)) 	; Big Integers(signed 64-bit)
		(7  (write-32bit-value stream (length data)) 	; String (UTF-8)
		    (write-utf-8-charseq stream data)) 		
		(8  (write-32-bit-float stream data)) 			; IEEE 754 floating point 32-bit representation
		(9  (write-64-bit-float stream data)) 			; IEEE 754 floating point 64-bit representation
		(10 (write-32-bit-decimal stream data)) 		; Custom signed decimal 32-bit representation
		(11 (write-64-bit-decimal stream data)) 		; Custom signed decimal 64-bit representation
		(12 (write-bcrypt-password stream data)) 		; Password hashed with bcrypt (fastest)
		(13 (write-scrypt-password stream data)) 		; Password hashed with scrypt
		(14 (write-argon2-password stream data)) 		; Password hashed with argon2 (slowest)
		(15 (write-date stream data)) 					; Date
		(16 (write-datetime stream data)) 				; Datetime
		(17 (write-timestamp stream data)) 				; Timestamp (unsigned 40-bit integer)
		(18 (write-time stream data)) 					; Time
		(19 (write-year stream data)) 					; Year
	)
	(file-position stream))

(defun write-rows (table-name rows)
	(let* ((tbl (gethash table-name
					(tables 
						(gethash *in-db* 
							*schemas*))))
		  (keypair)(datastream) (keystream)
		  (latest (make-array (length (fieldarr tbl)) :element-type t :initial-element :CLEAR)))	  
		(maphash (lambda (k v)							; Build a KEYPAIR string
			(declare (ignore v))
			(setf keypair (concatenate  'string
										(string k)
										"_")))
			(primary tbl))
		(setf keystream (open (concatenate  'string
											*data-dir*
											(string *in-db*) "/"
											keypair
											(string table-name) 
											".idx")
								:direction :output
								:element-type '(unsigned-byte 8)
								:if-exists :append))
		(setf datastream (open (concatenate 'string
											*data-dir*
											(string *in-db*) "/"  
											(string-downcase (string table-name))
											"_" (write-to-string (files tbl))
											".dat")
								:direction :io
								:element-type '(unsigned-byte 8)
								:if-exists :overwrite
								:if-does-not-exist :create))
		(unless (< (file-length datastream) 1)
			(file-position datastream (lastpos tbl))
			(read-row datastream tbl latest))
		(loop for row across rows do
			(setf (lastpos tbl) (file-position datastream))
			(loop for i below (length row) do
				(when (eql (aref row i) :auto)
					(setf (aref row i) (+ 1 (aref latest i))))
				(setf (aref latest i) (aref row i))
				(write-data datastream
							(aref row i)
							(datatype (aref (fieldarr tbl) i)))
				(when (eql (primary (aref (fieldarr tbl) i)) :TRUE)
					(write-data keystream
								(aref row i)
								(datatype (aref (fieldarr tbl) i)))))
			(write-8bit-value keystream (files tbl))
			(write-32bit-value keystream (lastpos tbl)))
			
		(close datastream)
		(close keystream)))
		

