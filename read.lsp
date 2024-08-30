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

(defun read-8bit-value (stream)
	(read-byte stream))

(defun read-bytes-to-bit-array (stream bytes)
	"Reads BYTES number of bytes from STREAM and returns a bit-array of size 8*BYTES"
	(let (	(read-byte)
			(bit-array (* 8 bytes) :element-type 'bit))
		(dotimes (l bytes)
			(setf read-byte (read-byte stream))
			(dotimes (i 8)
				(setf (aref bit-array (+ i (* 8 l)) (ldb (byte 1 (- 7 i)) bytes)))))
		bytes))

(defun read-16bit-value (stream)
	(let ((low-byte (read-byte stream))
			(high-byte (read-byte stream)))
			(+ (ash high-byte 8) low-byte)))


(defun read-64bit-value (stream)
	(+ (ash (read-byte stream) 0)
	   (ash (read-byte stream) 8)
	   (ash (read-byte stream) 16)
	   (ash (read-byte stream) 24)
	   (ash (read-byte stream) 32)
	   (ash (read-byte stream) 40)
	   (ash (read-byte stream) 48)
	   (ash (read-byte stream) 56)))

(defun read-64bit-big-endian (stream)
	"Read a 64-bit integer in big-endian format from an unsigned-byte 8 stream"
	(let ((value 0))
		(dotimes (i 8)
			(setf value (logior (ash value 8) (read-byte stream))))
		value))

(defun to-string (charseq)
	(with-output-to-string (seqstream)
    (loop for ch across charseq
          do (write-char ch seqstream))))

(defun utf-8-to-string (stream len)
	"Read a UTF-8 encoded string of length LEN from STREAM"
	(with-output-to-string (str)
		(dotimes (_ len)
			(write-char (code-char (decode-utf-8-from-stream stream)) str))
		str))
		
	
; Read signed bytes from the stream, to get also negative values etc.

(defun read-signed-byte-8 (stream)
	"Read a signed 8-bit integer from an unsigned-byte 8 stream"
	(let ((unsigned-value (read-byte stream)))
		(if (> unsigned-value 127)
			(- unsigned-value 256)
			unsigned-value)))
			
(defun read-signed-64bit-value (stream)
	"Read a signed 64-bit integer from an unsigned-byte 8 stream"
	(let ((unsigned-value (+(ash (read-byte stream) 0)
							(ash (read-byte stream) 8)
							(ash (read-byte stream) 16)
							(ash (read-byte stream) 24)
							(ash (read-byte stream) 32)
							(ash (read-byte stream) 40)
							(ash (read-byte stream) 48)
							(ash (read-byte stream) 56))))
							
    (if (>= (logand unsigned-value #x8000000000000000) 0) ; Check if the sign bit is set
        (- unsigned-value (ash 1 64))
        unsigned-value)))
		
(defun read-8bit-charseq (stream length)
	"Read a sequence of characters encoded as 8-bit values from the stream"
	(let ((charseq (make-array length :element-type 'character)))
		(dotimes (i length)
		(setf (aref charseq i) (code-char (read-8bit-value stream))))
		(to-string charseq)))
			
(defun read-16bit-charseq (stream length)
	"Read a sequence of characters encoded as 16-bit values from the stream"
	(let ((charseq (make-array length :element-type 'character)))
		(dotimes (i length)
		(setf (aref charseq i) (code-char (read-16bit-value stream))))
		(to-string charseq)))

; Floating-point numbers
(defun read-32-bit-decimal (stream)
	"Read a four byte-section known to be a 32-bit decimal representation"
	(32-bit-to-decimal (read-bytes-to-bit-array stream 4)))
	
(defun read-64-bit-decimal (stream)
	"Read a four byte-section known to be a 64-bit decimal representation"
	(64-bit-to-decimal (read-bytes-to-bit-array stream 8)))

(defun read-32-bit-float (stream)
	32-bit-to-float (read-bytes-to-bit-array stream 4))
	
(defun read-64-bit-float (stream)
	64-bit-to-float (read-bytes-to-bit-array stream 8))

; Passwords - salted, please
			
(defun read-argon2-password (stream password)
	"Returns t, if password is correct"
	(let (	(salt (make-array 16 :element-type '(unsigned-byte 8)))
			(hashed-pw (make-array 64 :element-type '(unsigned-byte 8))))
		(loop for i from 0 to 15 do
			(setf (aref salt i) (read-byte stream)))
		(loop for i from 0 to 63 do
			(setf (aref hashed-pw i) (read-byte stream)))
		(equalp hashed-pw (hash-argon2-password password salt))))
	
(defun read-scrypt-password (stream password)
	"Returns t, if password is correct"
	(let (	(salt (make-array 16 :element-type '(unsigned-byte 8)))
		(hashed-pw (make-array 24 :element-type '(unsigned-byte 8))))
	(loop for i from 0 to 15 do
		(setf (aref salt i) (read-byte stream)))
	(loop for i from 0 to 23 do
		(setf (aref hashed-pw i) (read-byte stream)))
	(equalp hashed-pw (hash-scrypt-password password salt))))
	
(defun read-bcrypt-password (stream password)
	"Returns t, if password is correct"
	(let (	(salt (make-array 16 :element-type '(unsigned-byte 8)))
			(hashed-pw (make-array 24 :element-type '(unsigned-byte 8))))
		(loop for i from 0 to 15 do
			(setf (aref salt i) (read-byte stream)))
		(loop for i from 0 to 23 do
			(setf (aref hashed-pw i) (read-byte stream)))
		(equalp hashed-pw (hash-bcrypt-password password salt))))

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