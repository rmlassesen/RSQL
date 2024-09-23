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

; Read table overview
; Section 1: 1 byte containing the number of rows, and then row names as a string of 8-bit chars
; Row names are additionally prefaced with an integer containing an integer containing the length of the name
; And then ends with the datatype enumeration for that row
; Section 2: Number of indexes(1 byte) then row names as 8-bit chars, that is used as indexes. Default is id.
; These row-names are similarly prefaced with an integer containing the length of the name, but not the datatype

; STRUCTURE
; Section_End_Pos(16-bit) Num_rows(8-bit) Row_length (8-bit) Row_name (8-bit charseq) Row_datatype (8-bit) 
; Num_indexes (8-bit) Row_length (8-bit) Row_name (8-bit charseq)

(defun read-table-form (table-name)
	(let ((rows '()) (indexes '()))
		(with-open-file (stream (concatenate 'string *data-dir* table-name ".tbl")
							:direction :input
							:element-type '(unsigned-byte 8))
			(loop for i from 0 below (read-byte stream) do 			 			; Iterate across the row names
				(push (cons 										 			; Push a CONS to ROWS
						(read-8bit-charseq stream (read-byte stream))			; with row name
						(read-byte stream)) rows))					 			; and datatype enumeration
			(loop for i from 0 below (read-byte stream) do						; Iterate across indexes
				(push (read-8bit-charseq stream (read-byte stream)) indexes))	; Push index row names to INDEXES
		(list 	table-name 														; Return a table-form
				(list-to-array (reverse rows))
				(list-to-array (reverse indexes))))))


(defun read-data-size (tbl_name idx)
	(with-open-file (stream (concatenate 'string "Lists/" idx "_" tbl_name ".tbl")
						  :direction :input
						  :element-type '(unsigned-byte 8))
		(read-64bit-value stream)))