; Helper functions for Hash-tables

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

; COPY-JLI is in actuality a deep hash-table copier		
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