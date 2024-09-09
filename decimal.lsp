; Custom decimal numbers
; The decimal datatype mimics the scientific notation, not by using a string
; But by simply defining the decimal placement with an exponent
; 1 bit is reserved as a sign-bit, to identify the polarity of the exponent
; 7 bits are reserved for the exponent, allowing an exponent of max 127
; 1 bit is reserved as a sign-bit, to identify the polarity of the value itself

(defun split-decimal-string (str)
	(let* (( valuestr (write-to-string str)) (pos (position #\. valuestr)))
		(list (subseq valuestr 0 pos) (subseq valuestr (+ pos 1)))))
		
(defun decimal-to-32-bit (value)
	"Convert a float into a 32-bit representation (single precision)"
	(let* (	(exponent)
			(bit-array (make-array 32 :element-type 'bit :initial-element 0))
			(valuelist (split-decimal-string value))
			(intstr (car valuelist)) 
			(decstr (car (cdr valuelist)))
			; Convert value into a combined integer with no decimal point, and parse it to a list of bits
			(valuebits (int-to-bit (abs (parse-integer (concatenate 'string intstr decstr))))))
		; If the integer-part is more than zero, use the length of this, to determine the decimal point placement
		; If not, find the first non-zero in the decimal part, to determine a negative exponent (10^-x)
		(when (< value 0) (setf (aref bit-array 8) 1))
		(when (< (length intstr) 8) (setf (aref bit-array 0) 1)) ; DEVELOP TODO Modify for positive exponent later
		(if (>= value 1)
			(setf exponent (logand (length decstr) #xFF))
			(setf exponent (logand (+ 2 (first-non-zero-in-string decstr)) #xFF))) ;Plus 1 for 0 before decimal point, plus 1 for index-start at 0
		(dotimes (i 7)
			(setf (aref bit-array (+ 1 i)) (logand (ash exponent (- i (- 7 1))) 1)))
		(loop for i from (- (length valuebits) 1) downto 0 
			for j from 0 do
				(setf (aref bit-array (- 31 j)) (aref valuebits i)))
		bit-array))

(defun decimal-to-64-bit (value)
	"Convert a float into a 32-bit representation (single precision)"
	(let* (	(exponent)
			(bit-array (make-array 64 :element-type 'bit :initial-element 0))
			(valuelist (split-decimal-string value))
			(intstr (car valuelist)) 
			(decstr (car (cdr valuelist)))
			; Convert value into a combined integer with no decimal point, and parse it to a list of bits
			(valuebits (int-to-bit (abs (parse-integer (concatenate 'string intstr decstr))))))
		; If the integer-part is more than zero, use the length of this, to determine the decimal point placement
		; If not, find the first non-zero in the decimal part, to determine a negative exponent (10^-x)
		(when (< value 0) (setf (aref bit-array 8) 1))
		(when (< (length intstr) 17) (setf (aref bit-array 0) 1)) ; DEVELOP TODO Modify for positive exponent later
		(if (>= value 1)
			(setf exponent (logand (length decstr) #xFF))
			(setf exponent (logand (+ 2 (first-non-zero-in-string decstr)) #xFF))) ;Plus 1 for 0 before decimal point, plus 1 for index-start at 0
		(dotimes (i 7)
			(setf (aref bit-array (+ 1 i)) (logand (ash exponent (- i (- 7 1))) 1)))
		(loop for i from (- (length valuebits) 1) downto 0 
			for j from 0 do
				(setf (aref bit-array (- 63 j)) (aref valuebits i)))
		bit-array))

(defun 32-bit-to-decimal (bit-array)
	(let (	(sign (aref bit-array 8))
			(signexp (aref bit-array 0))
			(exponent (bit-to-int bit-array 1 7))
			(value (bit-to-int bit-array 9 31)))
			
		(when (= signexp 1) (setf exponent (- 0 exponent)))
		(setf value (* value (expt 10 exponent)))
		(when (= sign 1) (setf value (- 0 value)))
		  value))