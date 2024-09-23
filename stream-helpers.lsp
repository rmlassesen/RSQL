(in-package :rsql)
; Helper functions for streams

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

(defun read-single-quote-string (stream char)
	"Read a string encapsulated by 'single-quotes'"
	(declare (ignore char))
	(let (	(str 	(make-array 0 	:element-type 'character			; Create an array to build a string in
									:fill-pointer 0
									:adjustable t)))
		(loop for c = (peek-char nil stream nil nil)
			while c do
				(cond 
					((eql c #\\)
						(file-position stream (+ 1 (file-position stream)))
						(if (eql (peek-char nil stream nil nil) #\')
							(vector-push-extend #\' str)
							(vector-push-extend #\\ str)))
					((eql c #\') (return))								; Return if the next character is a single quote, unless it's escaped
					(t (vector-push-extend c str)))
					(file-position stream (+ 1 (file-position stream))))
		(file-position stream (+ 1 (file-position stream)))
		str))
		
(defun read-non-alpha (stream)
	(let ((p (peek-char nil stream nil nil)) (r))
		(cond 
			((char= p #\") 		(read stream))
			((char= p #\') 		(read-char stream)						; Read char for a clean READ-SINGLE-QUOTE-STRING start
								(read-single-quote-string stream p))
			((char= p #\()		(read-char stream)						; Read char for a clean READ-CLAUSE start
								(read-clause stream))
			((typep p 'rcondoperators) 
								(setf r (read-char stream))
								(setf p (peek-char nil stream nil nil))
								(if (typep p 'rcondoperators)
									(read-from-string (format nil "~a~a" r (read-char stream)))
									(read-from-string (format nil "~a" r))))
			(t (error "Unexpected character ~a in CLAUSE" p)))))