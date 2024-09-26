(in-package :rsql)
; Write helper-functions

; Data-stream  is opened as an 8-bit stream
; Following function definitions are for writing 8-bit, 16-bit and 32-bit values to the stream
; Subsequently a function to write a sequence of chars as 8- and 16-bit to the stream  

(defun write-8bit-value (stream value)
	(write-byte value stream))

(defun write-16bit-value (stream value)
    (write-byte (ldb (byte 8 8) 	value) stream)
    (write-byte (ldb (byte 8 0) 	value) stream))

(defun write-32bit-value (stream value)
	"Write a 32-bit integer to an unsigned-byte 8 stream"
	(write-byte (ldb (byte 8 0)		value) stream)
	(write-byte (ldb (byte 8 8)		value) stream)
	(write-byte (ldb (byte 8 16)	value) stream)
	(write-byte (ldb (byte 8 24)	value) stream))

(defun write-typevalue-old (stream value elm-type)
	"Write a 30bit unsigned integer with a 2-bit type indicator (ELM-TYPE)"
	(when (or (> value 1073741824) (< value 0))
		(error "~a is out of range; must be 30-bit unsigned (max 1073741824)" value))
		
	(let (	(bit-array (make-array 32 	:element-type 'bit
									:initial-element 0))
			(value-bits (int-to-bit value)))
	; 0 [array], 1 for [hash-table], 2 for [string], 3 for [integer]
	(cond
		((eq elm-type 2) 	(setf (aref bit-array 0) 1))
		((eq elm-type 1)	(setf (aref bit-array 1) 1))
		((eq elm-type 0))	; 00 is already sat by :initial-element 0
		(t 					(setf (aref bit-array 0) 1)
							(setf (aref bit-array 1) 1)))
	(setf (subseq bit-array (- 32 (length value-bits))) (subseq value-bits 0))
	(write-custom-bit-array stream bit-array)))

(defun write-30bit-typevalue (stream value elm-type)
	"Write a 30bit unsigned integer with a 2-bit type indicator (ELM-TYPE)"
	(when (or (> value 1073741824) (< value 0))
		(error "~a is out of range; must be 30-bit unsigned (max 1073741824)" value))
	(write-byte (logior (ldb (byte 8 24) value) (ash elm-type 6)) stream)	; Set the first to bits to 00, 01, 10 or 11 based on ELM-TYPE
	(write-byte (ldb (byte 8 16)		value) stream)
	(write-byte (ldb (byte 8 8)	value) stream)
	(write-byte (ldb (byte 8 0)	value) stream))
	

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

(defun write-field-info (stream primary unique auto_increment nul)
	"Store fireld information in a single byte and write it to STREAM"
	(let ((b 0))
		(when (eql primary		  :TRUE) (incf b 1))
		(when (eql unique		  :TRUE) (incf b 2))
		(when (eql auto_increment :TRUE) (incf b 4))
		(when (eql nul			  :TRUE) (incf b 8))
		(write-byte b stream)))

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
			do (when (= (aref bit-array (+ i j)) 1)
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
	(write-custom-bit-array stream (decimal-to-32-bit value)))

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

; Datetime writing
(defun write-date (stream date-string)
	"Convert date string (YYYY-MM-DD) to 24-bit sequence and write it to stream"
	(let* (	(dt (date-from-string date-string))
			(dt-arr (make-array 24 	:element-type 'bit		; Make a 24-bit array
									:initial-element 0)))
			
			(setf (subseq dt-arr (- 11	(length (aref dt 0))) 11)	(aref dt 0)) ; Subtract actual number of bits with reserved bit-size, to get 
			(setf (subseq dt-arr (- 17	(length (aref dt 1))) 17)	(aref dt 1)) ; the offset for each bit-sequence
			(setf (subseq dt-arr (- 23	(length (aref dt 2))) 23)	(aref dt 2)) ; where 12 are reserved for YYYY and 6 is reserved for DD and MM
			(write-custom-bit-array stream dt-arr)))
			
(defun write-datetime (stream date-string)
	"Convert date string ( YYYY-MM-DDTHH:mm:ss.ms) to 48-bit sequence and write it to stream"
	(let* (	(dt (date-from-string date-string))
			(dt-arr (make-array 48 	:element-type 'bit		; Make a 48-bit array
									:initial-element 0)))
			
			(setf (subseq dt-arr (- 11	(length (aref dt 0))) 11)	(aref dt 0)) ; Subtract actual number of bits with reserved bit-size, to get 
			(setf (subseq dt-arr (- 17	(length (aref dt 1))) 17)	(aref dt 1)) ; the offset for each bit-sequence
			(setf (subseq dt-arr (- 23	(length (aref dt 2))) 23)	(aref dt 2)) ; where 12 are reserved for YYYY and 6 is reserved for DD and MM
			(setf (subseq dt-arr (- 28	(length (aref dt 3))) 28)	(aref dt 3)) ; 5 are reserved for HH and 6 is reserved for mm and ss 
			(setf (subseq dt-arr (- 34	(length (aref dt 4))) 34)	(aref dt 4)) ; while 7 are reserved for ms
			(setf (subseq dt-arr (- 40	(length (aref dt 5))) 40)	(aref dt 5))
			(setf (subseq dt-arr (- 47	(length (aref dt 6))) 47)	(aref dt 6))
			(write-custom-bit-array stream dt-arr)))