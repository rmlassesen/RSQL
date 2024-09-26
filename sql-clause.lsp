(in-package :rsql)
; SQL Clause handlier

; Clause structure: Hash-table with numbers as KEY
; The number represents the row-position of a ROW
; E.g. in a table of (ID, Username, Email), ID is 0 and Email is 2

; The value is an array where the first element (AREF arr 0) is initially NIL
; This NIL placeholder signifies that the value has not yet been read from the data source
; Once the row is evaluated and the actual value is needed for comparison, 
;  the value is stored in this position, reducing redundant reads in subsequent condition evaluations

; Example:
; "WHERE ID = 2" creates a CLAUSE HASH-TABLE with KEY 0. VALUE of KEY 0 is Array[NIL, = 2]
; The CLAUSE (= 2) is based of the datatype of ID as being an INT.
; If the data-type was VARCHAR: Array[NIL, equal-string "RuneM"]
; This is because a different function is needed to evaluate conditions of that data-type

; After the value is read and stored in the first position, the array could look like this:
; ["RuneM", equal-string "RuneM", (string> "SuperArch")]

; Multiple conditions are evaluated in sequence, short-circuiting on failure for efficiency

; Given that two ROWs are used in the same WHERE clause, like "WHERE username = email"
; The second appearing ROW will be referenced as a key :NUMBER
; E.g. for username Array[NIL, equal-string :2] (TODO: make equal-string function that reads from two streams at the same time)


(defun where-form (table-form stream)
	"Return a clause hash-table from WHERE clauses"
	(let (	(*readtable* *SQL-readtable*)
			(clause-list)
			(clauses (make-hash)))))
	
	
	
	
	
	
(defun test-w ()
	(let ((test-stream make-string-input-stream
		(concatenate 'string 
			"(id > 10 AND gender = 'Female') "
			"OR (first_name = 'Tybie' AND last_name = 'Brownstein') "
			"AND (email LIKE '%@wufoo.com' OR email IS NULL) "
			"AND (ip_address = '54.77.166.145' OR ip_address IS NOT NULL) "
			"AND (gender = 'Female' OR gender = 'Male')")))
			
	(where-form ((read-table-form "MOCK_DATA" test-stream)))))

(defvar *teststream*)
	
(defun testo ()
	(setf *teststream*
	( make-string-input-stream
		(concatenate 'string 
			"(id > 10 AND gender = 'Female') "
			"OR (first_name = 'Tybie' AND last_name = 'Brownstein') "
			"AND (email LIKE '%@wufoo.com' OR email IS NULL) "
			"AND (ip_address = '54.77.166.145' OR ip_address IS NOT NULL) "
			"AND (gender = 'Female' OR gender = 'Male')"))))
	
(defun testc ()
	(close *teststream*))
