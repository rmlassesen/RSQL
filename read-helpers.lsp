(in-package :rsql)
; Read Helper-functions


; Data-stream  is opened as an 8-bit stream
; Following function definitions are for reading 8-bit, 16-bit and 32-bit values from the stream
; Subsequently a function to read a sequence of chars as 8- and 16-bit from the stream

(defun read-8bit-value (stream)
	(read-byte stream))

(defun read-bytes-to-bit-array (stream bytes)
	"Reads BYTES number of bytes from STREAM and returns a bit-array of size 8*BYTES"
	(let ( (bt) (bit-array (make-array (* 8 bytes) :element-type 'bit)))
		(dotimes (l bytes)
			(setf bt (read-byte stream))
			(dotimes (i 8)
				(setf (aref bit-array (+ i (* 8 l))) (ldb (byte 1 (- 7 i)) bt))))
		bit-array))

(defun read-16bit-value (stream)
	(let ((low-byte (read-byte stream))
			(high-byte (read-byte stream)))
			(+ (ash high-byte 8) low-byte)))

(defun read-32bit-value (stream)
	(+ (ash (read-byte stream) 0)
	   (ash (read-byte stream) 8)
	   (ash (read-byte stream) 16)
	   (ash (read-byte stream) 24)))

(defun read-30bit-typevalue-old (stream)
	"Read 2bit type indicator and 30bit value from stream"
	(let ((bit-array (read-bytes-to-bit-array stream 4))) 	; Read 4 bytes from the stream into a bit array
		(cons 
			(bit-to-int 									; Read the value of the first two bits
				(subseq bit-array 0 2) 0 1)
			(bit-to-int 									; Read the value of the last 30 bits
				(subseq bit-array 2) 0 29))))

(defun read-30bit-typevalue (stream)
	"Read 2bit type indicator and 30bit value from stream"
	(let ((b (read-byte stream)))
		(cons 	(ldb (byte 8 6) b)
				(+ 	(ash (ldb (byte 6 0) b) 24)
					(ash (read-byte stream) 16)
					(ash (read-byte stream) 8)
					(read-byte stream)))))

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


(defun read-utf-8-charseq (stream len)
	"Read a UTF-8 encoded string of length LEN from STREAM"
	(let ((str (make-array len :element-type 'base-char :fill-pointer len)))
		(dotimes (i len)
			(setf (aref str i) (code-char (decode-utf-8-from-stream stream))))
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

(defun read-field-info (stream)
	"Retrieve fireld information from a single byte from  STREAM"
	(let ((info (make-array 4 :element-type 't :initial-element :FALSE))
		  (b (read-byte stream)))
		(when (logbitp 3 b)
			(setf (aref info 3) :TRUE))
		(when (logbitp 2 b)
			(setf (aref info 2) :TRUE))
		(when (logbitp 1 b)
			(setf (aref info 1) :TRUE))
		(when (logbitp 0 b)
			(setf (aref info 0) :TRUE))
		info))

		
(defun read-8bit-charseq (stream length)
	"Read a sequence of characters encoded as 8-bit values from the stream"
	(let ((charseq (make-array length :element-type 'character)))
		(dotimes (i length)
		(setf (aref charseq i) (code-char (read-8bit-value stream))))
		(coerce charseq 'string)))
			
(defun read-16bit-charseq (stream length)
	"Read a sequence of characters encoded as 16-bit values from the stream"
	(let ((charseq (make-array length :element-type 'character)))
		(dotimes (i length)
		(setf (aref charseq i) (code-char (read-16bit-value stream))))
		(coerce charseq 'string)))

; Floating-point numbers
(defun read-32-bit-decimal (stream)
	"Read a four byte-section known to be a 32-bit decimal representation"
	(32-bit-to-decimal (read-bytes-to-bit-array stream 4)))
	
(defun read-64-bit-decimal (stream)
	"Read a four byte-section known to be a 64-bit decimal representation"
	(64-bit-to-decimal (read-bytes-to-bit-array stream 8)))

(defun read-32-bit-float (stream)
	(32-bit-to-float (read-bytes-to-bit-array stream 4)))
	
(defun read-64-bit-float (stream)
	(64-bit-to-float (read-bytes-to-bit-array stream 8)))

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

; Datetime reading
(defun read-date (stream)
	"Read a 24-bit from stream and convert to date format YYYY-MM-DD"
	(let ((bit-array (read-bytes-to-bit-array stream 3))) 	; Read 3 bytes from the stream into a bit array
		(date-to-string	(bit-to-int bit-array 18 23) 		; Convert the 6 DD bits into an integer 	(DD)
						(bit-to-int bit-array 12 17) 		; Convert the 6 MM bits into an integer		(MM)
						(bit-to-int bit-array  0 11))))		; Convert the first 12 bits into an integer	(YYYY)
						
(defun read-datetime (stream)
	"Read a 48-bit from stream and convert to format YYYY-MM-DDTHH:mm:ss.ms"
	(let ((bit-array (read-bytes-to-bit-array stream 6))) 		; Read 6 bytes from the stream into a bit array
		(datetime-to-string	(bit-to-int bit-array 18 23) 		; Convert the 6 DD bits into an integer 	(DD)
							(bit-to-int bit-array 12 17) 		; Convert the 6 MM bits into an integer		(MM)
							(bit-to-int bit-array  0 11)		; Convert the first 12 bits into an integer	(YYYY)
							(bit-to-int bit-array 24 28)		; Convert the next  5 bits into an integer	(HH)
							(bit-to-int bit-array 29 34)		; Convert the next  6 bits into an integer	(mm)
							(bit-to-int bit-array 35 40)		; Convert the next  6 bits into an integer	(ss)
							(bit-to-int bit-array 41 47))))		; Convert the last  7 bits into an integer	(ms)
						