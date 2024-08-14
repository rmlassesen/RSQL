; Strings in UTF-8
; Encode charcode values from 0-127 in 1 byte	(1. byte prefixed with 0: 0xxxxxxx)		Remaining length 7-bit
; From 128-2047 in two bytes					(1. byte prefixed with 110: 110xxxxx)	Remaining length 11-bit
; From 2048-65535 in 3 bytes					(1. byte prefixed with 1110: 1110xxxx)	Remaining length 16-bit
; And All above 63535 in 4 bytes				(1. byte prefixed with 11110: 11110xxx)	Remaining length 21-bit
; 												Byte 2, 3 and 4 are prefixed with 10: 10xxxxxx

(defun unknown-char ()
	"Creates the #/? char in UTF-8"
	(make-array 1 :element-type '(simple-bit-vector 8) :initial-element 
		(make-array 8 :element-type 'bit :initial-contents '(0 1 1 1 1 1 1 0))))

(defun 1-byte-utf-8 (charcode)
	"Encode charcode values 0-127"
	(let* (	(byte1 (make-array 8 :element-type 'bit :initial-element 0))
			(bit-array (int-to-bit charcode))
			(i 1))
		(loop for b across bit-array do
			(setf (aref byte1 i) b)
			(incf i))
		(make-array 1 :element-type '(simple-bit-vector 8) :initial-element byte1)))
		
(defun 2-byte-utf-8 (charcode)
	"Encode charcode values 128-2047"
	(let* (	(bytes (make-array 2 :element-type '(simple-bit-vector 8) :initial-contents 
					(vector (make-array 8 :element-type 'bit :initial-contents '(1 1 0 0 0 0 0 0))
							(make-array 8 :element-type 'bit :initial-contents '(1 0 0 0 0 0 0 0))))) 
			(bit-array (int-to-bit charcode))
			(i 3)(j 0)) ; 3 is the index after the prefix in 1. byte, 0 is index of BYTES
			
		(loop for b across bit-array do
			(setf (aref (aref bytes j) i) b)
			(incf i)
			(when (= i 8)
				(setf i 2) ; 2 is the index after prefix in all other bytes
				(incf j))) ; Go to the next byte in bytes
		bytes))
		
(defun 3-byte-utf-8 (charcode)
	"Encode charcode values 2048-65535"
	(let* (	(bytes (make-array 3 :element-type '(simple-bit-vector 8) :initial-contents 
					(vector (make-array 8 :element-type 'bit :initial-contents '(1 1 1 0 0 0 0 0))
							(make-array 8 :element-type 'bit :initial-contents '(1 0 0 0 0 0 0 0))
							(make-array 8 :element-type 'bit :initial-contents '(1 0 0 0 0 0 0 0))))) 
			(bit-array (int-to-bit charcode))
			(i 4)(j 0)) ; 4 is the index after the prefix in 1. byte, 0 is index of BYTES
			
		(loop for b across bit-array do
			(setf (aref (aref bytes j) i) b)
			(incf i)
			(when (= i 8)
				(setf i 2) ; 2 is the index after prefix in all other bytes
				(incf j))) ; Go to the next byte in bytes
		bytes))
		
(defun 4-byte-utf-8 (charcode)
	"Encode charcode values 63536-1,114,111"
	(let* (	(bytes (make-array 4 :element-type '(simple-bit-vector 8) :initial-contents 
					(vector (make-array 8 :element-type 'bit :initial-contents '(1 1 1 1 0 0 0 0))
							(make-array 8 :element-type 'bit :initial-contents '(1 0 0 0 0 0 0 0))
							(make-array 8 :element-type 'bit :initial-contents '(1 0 0 0 0 0 0 0))
							(make-array 8 :element-type 'bit :initial-contents '(1 0 0 0 0 0 0 0))))) 
			(bit-array (int-to-bit charcode))
			(i 5)(j 0)) ; 5 is the index after the prefix in 1. byte, 0 is index of BYTES
			
		(loop for b across bit-array do
			(setf (aref (aref bytes j) i) b)
			(incf i)
			(when (= i 8)
				(setf i 2) ; 2 is the index after prefix in all other bytes
				(incf j))) ; Go to the next byte in bytes
		bytes))
		
(defun encode-utf-8 (char)
	"Encode character to UTF-8"
	(unless (or (characterp char)(integerp char))
		(format nil "~A is an un-recognized character value" char) ; TODO error-handling
		(return-from encode-utf-8 (unknown-char)))
	(let (	(charcode (if (numberp char)char (char-code char)))) ; Return byte-array based on the size of the charcode
		(cond 	((or (< charcode 0) (> charcode 1114111)) (unknown-char)) ; Return unknown-char if charcode is out of range
				((< charcode 128) (1-byte-utf-8 charcode))
				((and (> charcode 127)(> 2048 charcode))(2-byte-utf-8 charcode))
				((and (> charcode 2047)(> 65536 charcode))(3-byte-utf-8 charcode))
				((and (> charcode 65535)(> 1114111 charcode))(4-byte-utf-8 charcode)))))
				
(defun decode-utf-8-from-stream (stream)
	(let (	(bit-array)
			(bytes (make-array 1	
						:element-type '(simple-bit-vector 8)
						:adjustable t
						:fill-pointer 1
						:initial-element (byte-to-bits (read-byte stream)))))
		(loop for i from 0 to 3 do
			(if (= (aref (aref bytes 0) i) 1)
				(vector-push-extend (byte-to-bits (read-byte stream)) bytes)
				(return)))
		(setf bit-array (case (length bytes)
							(1 (make-array 7 
								:element-type 'bit 
								:initial-contents (concatenate 'vector (subseq (aref bytes 0) 1))))
							(2 (make-array 11 
								:element-type 'bit 
								:initial-contents (concatenate 'vector 
																(subseq (aref bytes 0) 3)
																(subseq (aref bytes 1) 2))))
							(3 (make-array 16 
								:element-type 'bit 
								:initial-contents (concatenate 'vector 
																(subseq (aref bytes 0) 4)
																(subseq (aref bytes 1) 2)
																(subseq (aref bytes 2) 2))))
							(4 (make-array 21 
								:element-type 'bit 
								:initial-contents (concatenate 'vector 
																(subseq (aref bytes 0) 5)
																(subseq (aref bytes 1) 2)
																(subseq (aref bytes 2) 2)
																(subseq (aref bytes 3) 2))))))
	(bit-to-int bit-array 0 (- (length bit-array) 1))))
								