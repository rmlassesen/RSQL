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

(load "read.lsp")
; Data-stream  is opened as an 8-bit stream
; Following function definitions are for writing 8-bit, 16-bit and 32-bit values to the stream
; Subsequently a function to write a sequence of chars as 8- and 16-bit to the stream  

(defun write-8bit-value (stream value)
	(write-byte value stream))

(defun write-16bit-value (stream value)
  (let ((high-byte (ldb (byte 8 8) value))
        (low-byte (ldb (byte 8 0) value)))
		
    (write-byte high-byte stream)
    (write-byte low-byte stream)))


(defun write-64bit-value (stream value)
	"Write a 64-bit integer to an unsigned-byte 8 stream"
	(write-byte (ldb (byte 8 0)		value) stream)
	(write-byte (ldb (byte 8 8)		value) stream)
	(write-byte (ldb (byte 8 16)	value) stream)
	(write-byte (ldb (byte 8 24)	value) stream)	
	(write-byte (ldb (byte 8 32)	value) stream)
	(write-byte (ldb (byte 8 40)	value) stream)	
	(write-byte (ldb (byte 8 48)	value) stream)
	(write-byte (ldb (byte 8 56)	value) stream))

(defun write-64bit-big-endian (stream value)
	"Write a 64-bit integer in big-endian format to an unsigned-byte 8 stream"
	(write-byte (ldb (byte 8 56)	value) stream)
	(write-byte (ldb (byte 8 48)	value) stream)
	(write-byte (ldb (byte 8 40)	value) stream)
	(write-byte (ldb (byte 8 32)	value) stream)	
	(write-byte (ldb (byte 8 24)	value) stream)
	(write-byte (ldb (byte 8 16)	value) stream)	
	(write-byte (ldb (byte 8 8)		value) stream)
	(write-byte (ldb (byte 8 0)		value) stream))

; Write signed bytes to the stream, if negative values are needed etc.

(defun write-signed-8bit-value (stream value)
	"Write a signed 8-bit integer to an unsigned-byte 8 stream"
	(when (or (< value -128) (> value 127))(error "~A Out of range for signed 8-bit integer" value)) 
	(write-byte (logand value #xFF) stream)) ; Make unsigned by masking with 0xFF


(defun write-signed-64bit-value (stream value)
	"Write a signed 64-bit integer to an unsigned-byte 8 stream"
	(when (or (< value -9223372036854775808) (> value 9223372036854775807))
		(error "~A Out of range for signed 64-bit integer" value))
	(let ((unsigned-value (logand value #xFFFFFFFFFFFFFFFF)))  ; Mask to 64 bits
		(write-byte (ldb (byte 8 0)  unsigned-value) stream)
		(write-byte (ldb (byte 8 8)  unsigned-value) stream)
		(write-byte (ldb (byte 8 16) unsigned-value) stream)
		(write-byte (ldb (byte 8 24) unsigned-value) stream)
		(write-byte (ldb (byte 8 32) unsigned-value) stream)
		(write-byte (ldb (byte 8 40) unsigned-value) stream)
		(write-byte (ldb (byte 8 48) unsigned-value) stream)
		(write-byte (ldb (byte 8 56) unsigned-value) stream)))

; Character sequences/Strings
(defun write-8bit-charseq (stream charseq)
	"Write a string as a 8-bit character sequence"
	(loop for ch across charseq
		do (write-8bit-value stream (char-code ch))))
		
(defun write-16bit-charseq (stream charseq)
	"Write a string as a 16-bit character sequence"
	(loop for ch across charseq
		do (write-16bit-value stream (char-code ch))))

(defun write-custom-bit-array (stream bit-array)
	"Write a custom bit-sequence (divisible by 8 (1 byte)) to an unsigned-byte 8 stream"
	(loop for i from 0 below (length bit-array) by 8 for newbyte = 0
		do (loop for j from 0 to 7
			do (when (aref bit-array (+ i j))
				(setf newbyte (logior newbyte (ash 1 (- 7 j))))))
		(write-byte newbyte stream)))
		
(defun write-utf-8-charseq (stream charseq)
	(loop for ch across charseq
		do	(loop for b across (encode-utf-8 ch)
				do (write-8bit-value stream (bit-to-int b)))))

; Floating-point numbers
(defun write-32-bit-float (stream value)
	"Write an IEEE 754 floating point 32-bit representation to an unsigned-byte 8 stream"
	(write-custom-bit-array stream (float-to-32-bit value)))
	
(defun write-64-bit-float (stream value)
	"Write an IEEE 754 floating point 64-bit representation to an unsigned-byte 8 stream"
	(write-custom-bit-array stream (float-to-64-bit value)))

(defun write-32-bit-decimal (stream value)
	"Write a 'custom decimal' 32-bit representation to an unsigned-byte 8 stream"
	(write-custom-bit-array stream (decimal-to-64-bit value)))

(defun write-64-bit-decimal (stream value)
	"Write a 'custom decimal' 64-bit representation to an unsigned-byte 8 stream"
	(write-custom-bit-array stream (decimal-to-64-bit value)))

; Passwords : Generate a random salt, salt password with KDF argon2i, scrypt or bcrypt

(defun write-argon2-password (stream password)
	"Generate a random salt, store salt continued by argon2i hashed password"
	(let* ((salt (make-salt)) (hashed-pw (hash-argon2-password password salt)))
		(loop for b across salt do
			(write-byte b stream))
		(loop for b across hashed-pw do
			(write-byte b stream))))
			
(defun write-scrypt-password (stream password)
	"Generate a random salt, store salt continued by argon2i hashed password"
	(let* ((salt (make-salt)) (hashed-pw (hash-scrypt-password password salt)))
		(loop for b across salt do
			(write-byte b stream))
		(loop for b across hashed-pw do
			(write-byte b stream))))
			
(defun write-bcrypt-password (stream password)
	"Generate a random salt, store salt continued by argon2i hashed password"
	(let* ((salt (make-salt)) (hashed-pw (hash-bcrypt-password password salt)))
		(loop for b across salt do
			(write-byte b stream))
		(loop for b across hashed-pw do
			(write-byte b stream))))

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
	(file-position stream)

)

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
	
		
		
		(loop for stream across idx-streams do (when stream (close stream))) ; Close each index-file-stream
))
	
