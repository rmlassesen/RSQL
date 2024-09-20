; Binary Helper functions
; Functions to help converting and handling bits and bytes, such as conversion to integers

(defun int-to-bit (value)
	"Convert an integer into bit-array, without any leading zero's"
	(read-from-string (format nil "#*~b" (abs value))))

(defun int-to-bit-leading-zeros (value)
	(let* (	(bit-array (int-to-bit value))
			(fl (multiple-value-bind (f r)			; Bind the floor-values from length and 8 to FL as cons
					(floor (length bit-array) 8)	; (car FL) represents the number of 8-bit sequences completed
						(cons f r)))				; (cdr FL) is the remainder, and represents how many zeros must be "added"
			(new-array (make-array (* 8 (+ 1 (car fl))) :element-type 'bit
														:initial-element 0)))
			(setf (subseq new-array (cdr fl)) (subseq bit-array 0))
			new-array))
			

(defun byte-to-bits (value)
	"Add leading zero's to a bit-array to complete a full byte"
	(let (	(bit-array (make-array 8 :element-type 'bit :initial-element 0))
			(bits (int-to-bit value)))
		(loop for i from (- (length bits) 1) downto 0 
			for j from 7 downto 0 do
				(setf (aref bit-array j) (aref bits i)))
		bit-array))

(defun bit-to-int-new (bit-array &optional (start 0) (end 7))
	(parse-integer (subseq (format nil "~a" bit-array) (+ 2 start) (+ 3 end)) :radix 2))

(defun bit-to-int (bit-array &optional (start 0) (end 7))
	"Convert part of an array of bits into an integer - default values 0 and 7 is for a full byte(8-bits)"
	(when (>= end (length bit-array))
		(error "Index ~A is out of range for array of size ~A" end (length bit-array)))
	(let ((value 0))
	(loop for i from end downto start
		for j from 0 do
			(when (= (aref bit-array i) 1)
				(setf value (+ value (expt 2 j)))))
	value))