; CSV as a datatype
; Creation of JSON-like-objects in Lisp, to capture the structure of CSV
; Possibility of importing these from .CSV files

; JSON-like-object - JSON Lisp Interpretation JLI (pronunciation: Jelly EN-UK)
; JavaScript Object Notation Lisp Interpretation

; JLI takes form as a simple array of hash-tables, representing each {},[] encapsulation with an array of hash-tables
		
(defun csv-record-to-list (stream)
	"Convert a CSV-record into a list"
	(let (	(str (read-line stream nil nil))
			(return-list '())
			(pos 0))
		(unless str (return-from csv-record-to-list nil))
		(loop for npos = (position #\, str :start pos)
			while npos do
				(setf return-list (append return-list
					(list (subseq str pos npos))))
				(setf pos (+ 1 npos)))
		(append return-list (list (subseq str pos)))))
		
(defun csv-set-to-list-array (stream)
	"Convert an entire set of CSV-records into and array of lists"
	(let ((csv-array (make-array 1	:element-type 'list
									:initial-element '()
									:fill-pointer 0
									:adjustable t)))
		(loop for record = (csv-record-to-list stream)
			while record do
				(vector-push-extend record csv-array))
		(setf (aref csv-array 0) 
			(map 'list (lambda (s) (read-from-string (concatenate 'string ":" s))) 
				(aref csv-array 0)))
		csv-array))
		
(defun import-csv-from-file (csv-file-path)
	"Open CSV-file, and return it as an array of lists"
	(let* (	(stream (open-file-without-bom csv-file-path))
			(ret (csv-set-to-list-array stream)))
		(close stream)
		ret))
		
(defun find-csv-record (csv-array identifier)
	(princ identifier)
	csv-array)

(defun csv-array-to-jli (csv-array)
	"Convert a CSV-array to JLI-object"
	(let (	(keys (aref csv-array 0))
			(jli (make-array 1	:element-type 'hash-table
								:initial-element (make-hash-table)
								:fill-pointer 0
								:adjustable t))
			(temp-list '())
			(temp-hash (make-hash-table)))
		(loop for i from 1 to (- (length csv-array) 1) do
			(setf temp-list (aref csv-array i))
			(loop for l from 0 to (- (length temp-list) 1) do
				(setf (gethash (nth l keys) temp-hash) (nth l temp-list)))
			(vector-push-extend (copy-hash-table temp-hash) jli)
			(clrhash temp-hash))
		jli))
	