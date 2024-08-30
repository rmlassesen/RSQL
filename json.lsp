; JSON as a datatype
; Creation of JSON-like-objects in Lisp
; Possibility of importing these from .json files
; Modelling a database-method of saving .json/JSON-like-objects

; JSON-like-object - JSON Lisp Interpretation JLI (pronunciation: Jelly EN-UK)
; JavaScript Object Notation Lisp Interpretation

; JLI takes form as a simple array of hash-tables, representing each {},[] encapsulation with an array of hash-tables

(declaim (ftype (function (string) string) import-as-string))
(declaim (ftype (function (stream) string) read-json-key))
(declaim (ftype (function (stream)) read-json-value))
(declaim (ftype (function (stream) hash-table) json-record-to-hash))
(declaim (ftype (function (string) array) make-jli-from-json))
(declaim (ftype (function (stream) array) make-jli-from-json-stream))
(declaim (ftype (function (string) array) jli-from-json-file))
(declaim (ftype (function (array keyword symbol string &optional list) array) find-in-jli))
(declaim (ftype (function (hash-table) string) json-string-from-jli-record))
(declaim (ftype (function (array) string) json-string-from-jli))


(defun import-as-string (json-file-path)
	"Opens a file and imports its contents as one string"
	(with-open-file (stream json-file-path
                        :direction :input
                        :element-type 'character)
		(let* ((buffer (make-array (file-length stream) :element-type 'character))
			(len (read-sequence buffer stream)))
		(subseq buffer 0 len))))
		

(defun pass-whitespaces (stream)
	(loop for c = (peek-char nil stream) do
		(if (typep c 'rwhitespaces)
			(read-char stream nil nil)
			(return))))
			
(defun read-json-key (stream)
	"Read substring from STREAM as key from current position until :, removing quotes"
	(pass-whitespaces stream)
	(let ((str (make-array '(0)	:element-type 'base-char
								:fill-pointer 0 
								:adjustable t)))
		(with-output-to-string (string-stream str)
			(write-char #\: string-stream)
			(loop for c = (read-char stream nil nil)
				while (and c (not (char= c #\:)))
				do (unless 
					(or (typep c 'rwhitespaces) (char= c #\"))
										(write-char c string-stream)))) 
		str))


(defun read-json-value (stream)
	"Read the value part of a JSON record"
	(pass-whitespaces stream)
	; If the value is identified as another data-set, recursively call jmake-jli-from-json-stream
	(when (char= (peek-char nil stream) #\[) (read-char stream nil nil) (return-from read-json-value (make-jli-from-json-stream stream)))
	; If the value is identified as another record, recursively call json-record-to-hash
	(when (char= (peek-char nil stream) #\{) (read-char stream nil nil) (return-from read-json-value (json-record-to-hash stream)))
	;If the value is identified as a string, use READ to return that string
	(when (char= (peek-char nil stream) #\") (return-from read-json-value (read stream)))
	(let ((str (make-array '(0)	:element-type 'base-char
							:fill-pointer 0 
							:adjustable t)))
		(with-output-to-string (string-stream str)
			(loop for c = (read-char stream nil nil)
				while (and c (not (char= c #\,)))
				do (unless (char= c #\,) (write-char c string-stream))))
		str))

(defun json-record-to-hash (stream)
	"Read a single JSON formated record, and return is a hash-table"
	(let ((jli-hash (make-hash-table)))
		(loop for c = (read-char stream nil nil)
			while (and c (not (char= c #\})))
				do (setf (gethash 
						(read-from-string (read-json-key stream))
						jli-hash)
						(read-json-value stream))
					(pass-whitespaces stream))
		jli-hash))

(defun make-jli-from-json (jsonString)
	"With a stringyfied JSON, create a stream to convert to JLI"
	(with-input-from-string (stream jsonString)
		(make-jli-from-json-stream stream)))

(defun make-jli-from-json-stream (stream)
	"Make an array of hash-tables, where each hash-table is a record of a JSON-object"
	(let ((jli (make-array 1 	:element-type 't 
								:fill-pointer 0
								:adjustable t)))	
		(loop for c = (read-char stream nil nil)
			while c do
				(when (char= c #\])
					(return))
				(when (char= c #\{)
					(vector-push-extend (json-record-to-hash stream) jli)))
		jli))
		
(defun jli-from-json-file (json-file-path)
	"Create a JLI-hash-table from .json-filetype"
	(make-jli-from-json (import-as-string json-file-path)))


; Return-list encapsules a list of :keywords as KEYS, of which entries from the records to return
; No return-list returns all/*

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
						
		
(defun json-string-from-jli-record (jli-record)
	"Stringify a JLI-record as JSON-format"
	(let ((json "{"))
		(maphash (lambda (k v)
					(cond 
						((hash-table-p v) 			(setf json (concatenate 'string json (format nil "\"~a\":~a," (symbol-name k)(json-string-from-jli-record v)))))
						((hash-table-p (aref v 0))	(setf json (concatenate 'string json (format nil "\"~a\":~a," (symbol-name k)(json-string-from-jli v)))))
						((string-is-number v)		(setf json (concatenate 'string json (format nil "\"~a\":~a," (symbol-name k) v))))
						(t 							(setf json (concatenate 'string json (format nil "\"~a\":\"~a\"," (symbol-name k) v))))))
					jli-record)
		(concatenate 'string (subseq json 0 (- (length json) 1)) "}")))
	
(defun json-string-from-jli (jli)
	(let ((json "["))
		(loop for record across jli do
			(setf json (concatenate 'string json (json-string-from-jli-record record) ",")))
		(concatenate 'string (subseq json 0 (- (length json) 1)) "]")))
		