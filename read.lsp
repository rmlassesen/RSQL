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

; Data-stream  is opened as an 8-bit stream
; Following function definitions are for reading 8-bit, 16-bit and 32-bit values from the stream
; Subsequently a function to read a sequence of chars as 8- and 16-bit from the stream

; Read table overview
; Section 1: Row names in string as 8-bit chars
; Row names are similarly prefaced with an integer containing the size/length of the name
; Section 2: Row names as 8-bit chars, that is used as indexes. Default is id.
; Section 3: Single 8-bit integer, no delimiter, identifies data-type of each row
; Each section is prefaced with an integer containing the size of the section

(defun read-table-form (tbl_name)
	(let ((section_length)(sections '())(file-path (concatenate 'string "Lists/" tbl_name ".tbl")))
		(with-open-file (stream file-path
						  :direction :input
						  :element-type '(unsigned-byte 8))
						  
			(setf  section_length(read-8bit-value stream)) ; Get length of first section
			(setf sections (list
				(loop while (> section_length (file-position stream))
					collect (read-8bit-charseq stream (read-8bit-value stream)))))
			(setf section_length (+ section_length (read-8bit-value stream))) ; Get length of second section and add to length of first section
			(setf sections (append sections (list
				(loop while (> section_length (file-position stream))
					collect (read-8bit-charseq stream (read-8bit-value stream))))))

			(setf sections (append sections (list
				(loop for byte = (read-byte stream nil) ; Read byte, return nil at EOF
					while byte ; Continue until EOF
					collect byte))))
		) 				
	)
)

(defun read-data-size (tbl_name idx)
	(with-open-file (stream (concatenate 'string "Lists/" idx "_" tbl_name ".tbl")
						  :direction :input
						  :element-type '(unsigned-byte 8))
		(read-64bit-value stream)))