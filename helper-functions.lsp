; Helper functions needed in several pieces of the code 

(deftype rwhitespaces () '(member #\Space #\Tab #\Newline #\Return))

(declaim (ftype (function (string) stream) open-file-without-bom))
(declaim (ftype (function (string) string) import-as-string))
(declaim (ftype (function (stream)) pass-whitespaces))
(declaim (ftype (function (string) boolean) string-is-number))
(declaim (ftype (function (symbol string string) boolean) compare-string-values))
(declaim (ftype (function (hash-table) hash-table) copy-hash-table))

; IMPORTANT function returns a STREAM and MUST BE CLOSED after use : (CLOSE STREAM)
(defun open-file-without-bom (file-path)
	"Open the file and return a stream with the BOM removed if present.
	If no BOM is present, return the stream positioned at the start"
	(let* ((stream (open file-path	:element-type '(unsigned-byte 8)
									:direction :input))
         (bom (make-array 3 :element-type '(unsigned-byte 8)
							:fill-pointer 0))
         (bom-str (progn
                    ; Read the first 3 bytes to check for BOM
					(setf (aref bom 0) (read-byte stream))
					(setf (aref bom 1) (read-byte stream))
					(setf (aref bom 2) (read-byte stream))	
					(format nil "~2x~2x~2x" (aref bom 0)(aref bom 1)(aref bom 2)))))
	; Close the stream, and open another with :element-type character
    ; then adjust file position based on BOM detection
	(close stream)
	(setf stream (open file-path	:element-type 'character
									:direction :input))
    (cond
      ((equal (subseq bom-str 0 3) "FEFF")	(file-position stream 2))	; UTF-16 BOM (Big Endian) 
      ((equal (subseq bom-str 0 3) "FFFE")	(file-position stream 2))	; UTF-16 BOM (Little Endian)
      ((equal bom-str "EFBBBF") 			(file-position stream 3)) 	; UTF-8 BOM
      (t 									(file-position stream 0)))	; No BOM
    stream))

(defun import-as-string (file-path)
	"Opens a file and imports its contents as one string"
	(with-open-file (stream file-path
                        :direction :input
                        :element-type 'character)
		(let* ((buffer (make-array (file-length stream) :element-type 'character))
			(len (read-sequence buffer stream)))
		(subseq buffer 0 len))))
		

(defun pass-whitespaces (stream)
	"Skip STREAM across members of rwhitespaces and return nothing"
	(loop for c = (peek-char nil stream) do
		(if (typep c 'rwhitespaces)
			(read-char stream nil nil)
			(return))))

(defun string-is-number (str)
	"Determines whether a string is entirely a number"
	(numberp (read-from-string str)))

(defun compare-string-values (operator x y)
	"Convert a simple operator to test on a string"
	(cond
		((and (string-is-number x) (string-is-number y)) ; If the strings are actually numbers, compare them as such
			(cond
				((eq operator '!=) 	(not (= (read-from-string x) (read-from-string y))))
				(t (funcall operator (read-from-string x) (read-from-string y)))))
		((eq operator '=)	(string= x y))
		((eq operator '>)	(string> x y))
		((eq operator '<)	(string< x y))
		((eq operator '<=)	(string<= x y))
		((eq operator '>=)	(string>= x y))
		((eq operator '!=)	(not (string= x y)))
		(t nil)))

		
(defun copy-hash-table (hash-tbl)
	"Returns a deep copy of HASH-TBL, recursively copying any hash tables within it"
	(let ((new-hash (make-hash-table 
					:test (hash-table-test hash-tbl)
					:rehash-size (hash-table-rehash-size hash-tbl)
					:rehash-threshold (hash-table-rehash-threshold hash-tbl)
					:size (hash-table-size hash-tbl))))
		(maphash (lambda (k v)
			(setf (gethash k new-hash)
				(cond
					((hash-table-p v) 			(copy-hash-table v))	; If the key is a record/hash-table, copy it
					((hash-table-p (aref v 0))	(copy-jli v)) 			; Recursively copy a JLI object (array of hash-tables)
					(t 							v)))) 					; Otherwise, copy the value directly
			hash-tbl)
		new-hash))