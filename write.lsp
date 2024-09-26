(in-package :rsql)
; Data-types: 
; 0		Index-integer (64-bit unsigned big endian integer)  (NULL is represented with 64 1-ones (2^64))
; 1		Bytes(unsigned 8-bit)								(BYTES can't be NULL, but 00000000 or 11111111 is okay)
; 2		Integers(signed 64-bit)								(NULL is represented with 64 1-ones (2^64))
; 3		Positive Only Integers(unsigned 64-bit)				(NULL is represented with 64 1-ones (2^64))
; 4		Char-sequences/String(UTF-8)						(NULL is represented with character code 0: #\Nul)
; 5		IEEE 754 floating point 32-bit representation		(NULL is represented with 32 1-ones (2^32))
; 6		IEEE 754 floating point 64-bit representation		(NULL is represented with 64 1-ones (2^64))
; 7		Custom signed decimal 32-bit representation			(NULL is represented with 32 1-ones (2^32))
; 8		Custom signed decimal 64-bit representation			(NULL is represented with 64 1-ones (2^64))
; 9		Password hashed with bcrypt							(PASSWORDS cannot be NULL)
; 10	Password hashed with scrypt							(PASSWORDS cannot be NULL)
; 11	Password hashed with argon2i						(PASSWORDS cannot be NULL)
; 12	Date												(NULL is represented by 0000-00-00 - effectively all bytes are zero)
; 13	Datetime											(NULL is represented by 0000-00-00T00:00:00.00  - effectively all bytes are zero)
; 14	Timestamp											(NULL is represented by 40 1-ones (2^40))
; 15	Time												(NULL is represented by 00:00:00.00  - effectively all bytes are zero)
; 16	Year												(NULL is 0000  - effectively all bytes are zero)

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


(declaim (ftype (function (t)) write-table-form))

(defun write-table-form (table-form)
	"Write .tbl file with information about the specific table"
	(with-open-file (stream (concatenate 'string 
										 *data-dir*
										 (string *in-db*) "/"
										 (string (name table-form)) 
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
		(let ((pairs ""))
			(write-8bit-value stream 						; Write the number of PRIMARY KEY fields
			(hash-table-count (primary table-form)))		; (Preferably only 1, but more if the PRIMARY KEY is made up of multiple columns)
			(maphash (lambda (k v)
				(setf pairs 								; Construct PRIMARY KEY index file-name
					(concatenate 'string pairs (string k) "_"))
				(write-8bit-value stream v))				; Write each ROWNUM
				(primary table-form))						; Create an indexing file for the PRIMARY KEY
			(with-open-file (stream (concatenate 'string *data-dir* (string *in-db*) "/" pairs (string (name table-form)) ".idx")
				:direction :output
				:if-exists :supersede)))
			
		(write-8bit-value stream 							; Write the number of INDEXES fields
			(hash-table-count (indexes table-form)))			
		(maphash (lambda (k v)								; Write each ROWNUM
			(write-8bit-value stream v)						; Create an indexing file for each index
			(with-open-file (stream (concatenate 'string *data-dir* (string *in-db*) "/" k "_" (string (name table-form)) ".idx")
							:direction :output
							:if-exists :supersede)))
			(indexes table-form))))	


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

