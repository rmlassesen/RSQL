; Data-types: 
; 0		Index-integer (64-bit unsigned big endian integer) 
; 1		Bytes(unsigned 8-bit)
; 2		Integers(signed 64-bit)
; 3		Positive Only Integers(unsigned 64-bit)
; 4		Char-sequences/String(unsigned 16-bit)
; 5		IEEE 754 floating point 32-bit representation
; 6		IEEE 754 floating point 64-bit representation
; 7		Custom signed decimal 32-bit representation
; 8		Custom signed decimal 64-bit representation
; 9		Password hashed with bcrypt
; 10	Password hashed with scrypt
; 11	Password hashed with argon2i


; Write table overview
; Section 1: Row names in string as 8-bit chars
; Row names are similarly prefaced with an integer containing the size/length of the name
; Section 2: Row names as 8-bit chars, that is used as indexes. Default is id.
; Section 3: Single 8-bit integer, no delimiter, identifies data-type of each row
; Each section is prefaced with an integer containing the size of the section

(defun row-names-size (rows)
  "Calculate the total byte-size for Section"
  (+ (apply #'+ (mapcar #'length rows)) (length rows) 1))

(defun write-table-form (tbl_name rows idx dtype)
	"Write .tbl file with information about the specific table"
	(let (	(file-path (concatenate 'string "Lists/" tbl_name ".tbl")))
		(with-open-file (stream file-path
							:direction :output
							:if-exists :supersede
							:element-type '(unsigned-byte 8))
							
			(write-8bit-value stream (row-names-size rows)) ; Write the length of Section 1
			(dolist (r rows)
				(write-8bit-value stream (length r))
				(write-8bit-charseq stream r))
			(write-8bit-value stream (row-names-size idx)) ; Write the length of Section 2
			(dolist (r idx)
				(write-8bit-value stream (length r))
				(write-8bit-charseq stream r))
			(dolist (tp dtype)
				(write-8bit-value stream tp)))
		
		(dolist (r idx)
			(with-open-file (stream (concatenate 'string "Lists/" r "_" tbl_name ".idx")
								:direction :output
								:if-exists :supersede
								:element-type '(unsigned-byte 8))
								(write-64bit-value stream 0))))) ; Create an indexing file, and preface it with 0, as a data-file-size designator					


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
		(4 (write-16bit-charseq stream data)) 		; String (16-bit)
		(5 (write-32-bit-float stream data)) 		; IEEE 754 floating point 32-bit representation
		(6 (write-64-bit-float stream data)) 		; IEEE 754 floating point 64-bit representation
		(7 (write-32-bit-decimal stream data)) 		; Custom signed decimal 32-bit representation
		(8 (write-64-bit-decimal stream data)) 		; Custom signed decimal 64-bit representation
		(9 (write-bcrypt-password stream data)) 	; Password hashed with bcrypt (fastest)
		(10 (write-scrypt-password stream data)) 	; Password hashed with scrypt
		(11 (write-argon2-password stream data)) 	; Password hashed with argon2 (slowest)
	)
	(file-position stream))

(defun write-row (tbl_name num-rows data)
	"Write a row/line to the database file of table(tbl_name)"
	(let* (	(table-form tbl_name)
			(rows (first table-form)) 
			(idx (second table-form)) 
			(dtype (third table-form))
			(idx-streams (make-array (length idx)))
			(data-size (read-data-size tbl_name (first idx))))
		
		(format t "Unused variables ~A ~A ~A ~A" data num-rows rows dtype)
		(let ((i 0))
			(dolist (r idx)
				(setf (aref idx-streams i) (open (concatenate 'string "Lists/" r "_" tbl_name ".idx")
											:direction :output
											:if-exists :append
											:element-type '(unsigned-byte 8))) ; Open a file-stream for each index-file
				(incf i)))
		(with-open-file (datastream (concatenate 'string "Lists/data" (data-file-number data-size) "_" tbl_name ".idx")
											:direction :output
											:if-exists :append
											:element-type '(unsigned-byte 8))
											
											
											
											)
	
		
		
		(loop for stream across idx-streams do (when stream (close stream))))) ; Close each index-file-stream
