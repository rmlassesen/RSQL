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

; Data-stream  is opened as an 8-bit stream
; Following function definitions are for reading 8-bit, 16-bit and 32-bit values from the stream
; Subsequently a function to read a sequence of chars as 8- and 16-bit from the stream

(defun read-data (stream data-type &optional data)
	(when (>= (file-position stream) (file-length stream))
		(return-from read-data nil))
	(case data-type	
		(0 (read-64bit-big-endian stream))		; Index-integer (unsigned 64-bit big endian)
		(1 (read-8bit-value stream)) 			; Byte (8-bit)
		(2 (read-64bit-value stream)) 			; Signed Integer (64-bit)
		(3 (read-signed-64bit-value stream)) 	; Unsigned Integer (64-bit)
		(4 (read-utf-8-charseq stream 			; String (UTF-8)
				(read-32bit-value stream))); 
		(5 (read-32-bit-float stream)) 			; IEEE 754 floating point 32-bit representation
		(6 (read-64-bit-float stream)) 			; IEEE 754 floating point 64-bit representation
		(7 (read-32-bit-decimal stream)) 		; Custom signed decimal 32-bit representation
		(8 (read-64-bit-decimal stream)) 		; Custom signed decimal 64-bit representation
		(9 (read-bcrypt-password stream data)) 	; Password hashed with bcrypt (fastest)
		(10 (read-scrypt-password stream data)) ; Password hashed with scrypt
		(11 (read-argon2-password stream data)) ; Password hashed with argon2 (slowest)
	))
	
(defun skip-data (stream data-type)
	(when (>= (file-position stream) (file-length stream))
		(return-from skip-data nil))
	(case data-type	
		(0 (file-position stream (+ 8 (file-position stream))))		; Index-integer (unsigned 64-bit big endian)
		(1 (file-position stream (+ 1 (file-position stream)))) 	; Byte (8-bit)
		(2 (file-position stream (+ 8 (file-position stream)))) 	; Signed Integer (64-bit)
		(3 (file-position stream (+ 8 (file-position stream)))) 	; Unsigned Integer (64-bit)
		(4 (file-position stream (+ (read-32bit-value stream)  		; String (UTF-8)
									(file-position stream))))
		(5 (file-position stream (+ 4 (file-position stream)))) 	; IEEE 754 floating point 32-bit representation
		(6 (file-position stream (+ 8 (file-position stream)))) 	; IEEE 754 floating point 64-bit representation
		(7 (file-position stream (+ 4 (file-position stream)))) 	; Custom signed decimal 32-bit representation
		(8 (file-position stream (+ 8 (file-position stream)))) 	; Custom signed decimal 64-bit representation
		(9 (file-position stream (+ 40 (file-position stream)))) 	; Password hashed with bcrypt (fastest)
		(10 (file-position stream (+ 40 (file-position stream))))	; Password hashed with scrypt
		(11 (file-position stream (+ 80 (file-position stream))))	; Password hashed with argon2 (slowest)
	))


; Read table overview
; Section 1: 1 byte containing the number of rows, and then row names as a string of 8-bit chars
; Row names are additionally prefaced with an integer containing an integer containing the length of the name
; And then ends with the datatype enumeration for that row
; Section 2: Number of indexes(1 byte) then row names as 8-bit chars, that is used as indexes. Default is id.
; These row-names are similarly prefaced with an integer containing the length of the name, but not the datatype

; STRUCTURE
; Section_End_Pos(16-bit) Num_rows(8-bit) Row_length (8-bit) Row_name (8-bit charseq) Row_datatype (8-bit) 
; Num_indexes (8-bit) Row_length (8-bit) Row_name (8-bit charseq)

(defun read-table-form (schema-name table-name)
	(let ((rows '()) (indexes '()))
		(with-open-file (stream (concatenate 'string *data-dir* (string schema-name) "/" (string table-name) ".tbl")
							:direction :input
							:element-type '(unsigned-byte 8))
			(loop for i from 0 below (read-byte stream) do 			 			; Iterate across the row names
				(push (cons 													; Push a CONS to ROWS
						(read-from-string
							(read-8bit-charseq stream (read-byte stream)))		; with row name
						(read-byte stream)) rows))					 			; and datatype enumeration
			(loop for i from 0 below (read-byte stream) do						; Iterate across indexes
				(push (read-8bit-charseq stream (read-byte stream)) indexes))	; Push index row names to INDEXES
		(list 	(list-to-array (reverse rows)) 									; Return a table-form
				(list-to-array (reverse indexes))))))


; Read the indexing file to determine the number of files and rows used for a specific table.
; The indexing file begins with an 8-bit integer representing the file number.
; Each entry includes:
;			- The field value
; 			- An 8-bit integer indicating the file number
; 			- A 32-bit integer representing the file position 
;			 (allowing files up to 4MB).

;READ-DATA-SIZE reads the file number and iterates through the file, skipping data based on the field’s data type
; and advancing 5 bytes per entry (1 byte for the file number and 4 bytes for the file position).


(defun read-data-size (schema-name table-name key)
	"Returns a CONS of file number and ROW COUNT
	KEY must be a CONS of FIELD NAME and DATA-TYPE"
	(with-open-file 
		(stream (concatenate 'string 
							 *data-dir*
							 schema-name "/" 
							 (car key) "_" 
							 table-name ".tbl")
						:direction :input
						:element-type '(unsigned-byte 8))
		(cons 	(read-8bit-value stream)
				(loop	for i from 0
						for d = (skip-data stream (cdr key))
						while d do 
							(file-position stream (+ 5 (file-position stream)))
						finally (return i)))))