; JSON-like-object - JSON Lisp Interpretation JLI (pronunciation: Jelly EN-UK)
; JavaScript Object Notation Lisp Interpretation

; JLI takes form as a simple array of hash-tables, representing each {},[] encapsulation with an array of hash-tables

(declaim (ftype (function (array) array) copy-jli))
(declaim (ftype (function (array keyword symbol string &optional list) array) find-in-jli))

(defun copy-jli (jli)
	"Return an exact copy of a JLI"
	(let (	(new-jli (make-array (length jli) :element-type 't))
			(i -1))
			
		(loop for record across jli do
			(incf i)
			(setf (aref new-jli i)
				(cond
					((hash-table-p record) 			(copy-hash-table record))	; If the record is actually a record, copy it
					((hash-table-p (aref record 0))	(copy-jli record)) 			; Recursively copy a JLI object (array of hash-tables)
					(t 								record)))) 					; Otherwise, copy the value directly
		new-jli))
		

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
		
(defun jli-entry (key jli &optional record-number)
	"Return entry from record in JLI"
	(if record-number
		(gethash key (aref jli record-number))
		(gethash key (aref jli 0))))