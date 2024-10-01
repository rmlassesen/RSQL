(in-package :rsql)
; JSON-like-object - JSON Lisp Interpretation JLI (pronunciation: Jelly EN-UK)
; JavaScript Object Notation Lisp Interpretation

; JLI takes form as a simple array of hash-tables, representing each {},[] encapsulation with an array of hash-tables

; Return-list encapsulates a list of :keywords as KEYS, of which entries from the records to return
; No return-list returns all/* (temp function for DEV)

(defun find-in-jli (jli key operator val &optional return-list)
	"Return a JLI, containing all records that matches conditions"
	(let ((jli-table (make-array 0 	:element-type 'array
									:fill-pointer 0
									:adjustable t))
			(temp-hash (make-hash-table)))
		; If RETURN-LIST is parsed, return selected key/value pairs	
		(when return-list
			(loop for record across jli do
				(when (compare-string-values operator (gethash key record) val)
					(dolist (k return-list)
						(setf (gethash k temp-hash) (gethash k record)))
					(vector-push-extend (copy-hash-table temp-hash) jli-table)
					(clrhash temp-hash)))
			(return-from find-in-jli jli-table))
		; If no RETURN-LIST is parsed, return all key/value pairs							
		(loop for record across jli do
			(when (compare-string-values operator (gethash key record) val)
				(vector-push-extend record jli-table)))
		jli-table))
		
(defun jli-entry (jli key &optional record-number)
	"Return entry from record in JLI"
	(if record-number
		(gethash key (aref jli record-number))
		(gethash key (aref jli 0))))
		

; JLI DataType brainstorm 
; Binary representation of an array: [length][reference][reference][...]
; [length] 		Number of references, total length in bytes
; [reference] 	32-bit unsigned integer
; [integer]		64-bit signed
; [string]		[length] Total bytes for string	
;				[UTF-8] encoded charseq
; [hash-table]	[length][KEY]/[VALUE]
; [key]			[string] representation of key
; [value] 		2 bits giving the four possible values 0, 1, 2, 3, 22 bits for the length
;				0 for another [array], 1 for [hash-table], 2 for [string], 3 for [integer]

; Get length of array for number of references
; Loop across JLI array
; For each element, return file-position to indexing array
; Arrays are created recursively - so indexing-array should not be part of root function

(defun write-string-to-stream (str stream)
		(write-30bit-typevalue stream (length str) 2)
		(write-utf-8-charseq stream str))

(defun write-element (elm stream)
	(cond
		((typep elm 'string) 	(write-string-to-stream elm stream))
		((typep elm 'hash-table)(write-hash-table elm stream))
		((typep elm 'array)		(write-array elm stream))
		(t 						(write-30bit-typevalue stream elm 3))))

(defun write-hash-table (ht stream)
	"Write a Hash-table to the stream"
	(write-30bit-typevalue stream (hash-table-count ht) 1)
	(maphash (lambda (key value)
		(setf key (from-keyword key))
		(write-8bit-value stream (length key))
		(write-utf-8-charseq stream key)
		(write-element value stream))
		ht))

(defun write-array (arr stream)
	"Write an array to stream"
	(write-30bit-typevalue stream (length arr) 0)
	(loop for e across arr do
		(write-element e stream)))

(defun write-jli (jli file-path)
	"Write the contents of a JLI-object to a (.jli) file"
	(let ((stream (open file-path	:direction :output 
									:if-exists :supersede
									:element-type '(unsigned-byte 8))))
		(write-array jli stream)							
		(close stream)))

(defun read-element (stream)
	(let ((typevalue (read-30bit-typevalue stream)))
	(case (car typevalue)
		(2	(read-utf-8-charseq stream (cdr typevalue)))
		(1	(read-hash-table stream (cdr typevalue)))
		(0	(read-array stream (cdr typevalue)))
		(3 	(read-32bit-value stream)))))
		
(defun read-hash-table (stream ht-size)
	"Read a hash-table from the stream"
	(let ((new-ht  (make-hash-table)))
		(loop for _ from 0 below ht-size do
			(setf (gethash 
					(to-keyword 
						(read-utf-8-charseq stream 
							(read-8bit-value stream)))
					new-ht)
					(read-element stream)))
		new-ht))

(defun read-array (stream arr-size)
	"Read an array from stream"
	(let* ((arr (make-array arr-size :element-type t)))
		(loop for i from 0 below (length arr) do
			(setf (aref arr i) (read-element stream)))
		arr))

(defun read-jli (file-path)
	"Read the contents of a JLI-object from a (.jli) file"
	(let* (	(stream (open file-path	:direction :input 
									:element-type '(unsigned-byte 8)))
			(jli (read-element stream)))							
		(close stream)
		jli))

				
(defun test-read-jli ()
	"Testing read-jli and accesory functions"
	(read-jli "Lists/test.jli"))
	
(defun utf-test ()
	(let ((fsize))
		(with-open-file (stream "MockData/MOCK_DATA.json"	:direction :input 
														:element-type '(unsigned-byte 8))
			(setf fsize (- (file-length stream) 1))
			(time
				(loop for c = (decode-utf-8-from-stream stream)
					while (< (file-position stream) fsize) do
						(format nil "~a" c))))
		(with-open-file (stream "MockData/MOCK_DATA.json"	:direction :input 
															:element-type 'character)
			(time
				(loop for c = (read-char stream nil nil)
					while (< (file-position stream) fsize) do
						(format nil "~a" c))))))