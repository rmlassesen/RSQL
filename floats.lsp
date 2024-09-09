; Floating-point numbers
(defun dec-to-bit (value mantissa)
	"Convert a decimal part into bits"
	(let ((bit-array (make-array mantissa :element-type 'bit :initial-element 0)) (i 0))
	; Make an initial binary representation of the decimal part
		(loop while (< i mantissa) do
			(when (= (setf value (* value 2)) 0) (return)) ; Multiply value with 2 and return if the result is zero
			(when (>= value 1)
				(setf (aref bit-array i) 1)
				(decf value))
			(incf i))
	; Duplicate the initial binary representation until a representation of the mantissa size is completed
		(loop while (< i mantissa) for j from 0 do
			(setf (aref bit-array i) (aref bit-array j))
			(incf i))
		bit-array))

			
(defun make-32-bit-exponent (value int-bits dec-bits)
	(let (	(exponent (make-array 8 :element-type 'bit :initial-element 0))
			(exp-bias (int-to-bit (+	127 ; Bias is 127 for 32-bit and 1023 for 64-bit
						(if (< (abs value) 1)
							(- -1 (position 1 dec-bits))
							(- (length int-bits) 1))))))
			(if (= 8 (length exp-bias))
				exp-bias
				(progn
					(loop for i from (- (length exp-bias) 1) downto 0
						for j from 0
						do (setf (aref exponent (- 7 j)) (aref exp-bias i)))
					exponent))))
					
(defun make-64-bit-exponent (value int-bits dec-bits)
	(let (	(exponent)
			(exp-bias (int-to-bit (+	1023 ; Bias is 127 for 32-bit and 1023 for 64-bit
						(if (< (abs value) 1)
							(- -1 (position 1 dec-bits))
							(- (length int-bits) 1))))))
			(if (= 11 (length exp-bias))
				exp-bias
				(progn
					(setf exponent (make-array 11 :element-type 'bit :initial-element 0))
					(loop for i from (- (length exp-bias) 1) downto 0
						for j from 0
						do (setf (aref exponent (- 10 j)) (aref exp-bias i)))
					exponent))))
	

(defun float-to-32-bit (value)
	"Convert a float into a 32-bit representation (single precision)"
	(multiple-value-bind (int dec)
		(truncate (abs value)) ; Truncate value into int and dec
		(let (	(i 1)	
				(bits (make-array 32 :element-type 'bit))
				(int-bits (int-to-bit int))
				(dec-bits (dec-to-bit dec 22))) ; Mantissa size 23 for 32-bit (minus 1 for min integer of 0)
			(setf (aref bits 0) (if (< value 0) 1 0))
			(loop for b across (make-32-bit-exponent value int-bits dec-bits) do
				(setf (aref bits i) b)
				(incf i))
			(loop for l from 1 to (- (length int-bits) 1) do ; Skip the implied 1-bit by starting at 1
				(setf (aref bits i) (aref int-bits l))
				(incf i))
			(loop for b across dec-bits do
				(setf (aref bits i) b)
				(incf i)
				(when (= 32 i) (return-from float-to-32-bit bits))))))			

(defun float-to-64-bit (value)
	"Convert a float into a 32-bit representation (single precision)"
	(multiple-value-bind (int dec)
		(truncate (abs value)) ; Truncate value into int and dec
		(let (	(i 1)
				(bits (make-array 64 :element-type 'bit))
				(int-bits (int-to-bit int))
				(dec-bits (dec-to-bit dec 51))) ; Mantissa size 51 for 64-bit (minus 1 for min integer of 0)
			(setf (aref bits 0) (if (< value 0) 1 0))
			(loop for b across (make-64-bit-exponent value int-bits dec-bits) do
				(setf (aref bits i) b)
				(incf i))
			(loop for l from 1 to (- (length int-bits) 1) do ; Skip the implied 1-bit by starting at 1
				(setf (aref bits i) (aref int-bits l))
				(incf i))
			(loop for b across dec-bits do
				(setf (aref bits i) b)
				(incf i)
				(when (= 64 i) (return-from float-to-64-bit bits))))))	

(defun calculate-mantissa (bit-array)
	(apply '+ (loop for b across bit-array
				for i from 1
				when (= b 1)
				collect (expt 2 (- i)))))
				
(defun 32-bit-to-float (bit-array)
	"Convert a read 32-bit bit-array to a float"
	(let (	(sign (aref bit-array 0))
			(exponent (- (bit-to-int bit-array 1 8) 127))
			(mantissa (calculate-mantissa (subseq bit-array 9))))
		(format t " Sign: ~A ~% Exponent: ~A ~% Mantissa: ~A ~%" sign exponent mantissa)
		; Formula: -1^sign * (1 + mantissa) * 2^exponent
		(* (expt -1 sign) (+ 1 mantissa) (expt 2 exponent))))	
				

(defun 64-bit-to-float (bit-array)
	"Convert a read 64-bit bit-array to a float"
	(let (	(sign (aref bit-array 0))
			(exponent (- (bit-to-int bit-array 1 11) 1023))
			(mantissa (calculate-mantissa (subseq bit-array 12))))
			
				(format t " Sign: ~A ~% Exponent: ~A ~% Mantissa: ~A ~%" sign exponent mantissa)
		; Formula: -1^sign * (1 + mantissa) * 2^exponent
		(* (expt -1 sign) (+ 1 mantissa) (expt 2 exponent))))