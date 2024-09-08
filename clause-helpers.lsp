; (SQL) Clause Helper functions

(defun to-clause-list (a b c)
	(if (or (eql '<> b) 	; If the opererator is a NOT variant (!= or <>) insert NOT
			(eql '!= b)
			(eql 'not b))
			(list 'not (list '= a c))
			(list b a c)))

(defun clean-clause-list (a b c)
	"Take three elements of a clause and arrange them proberby into a list
	E.g. A = B -> (= A B)"
	(list b a c))

(defun clean-clause (clause-array)
	"Turn a read-clause array into an actual condition"
	(let (	(clause-list '())
			(clause-collection (make-array 3 :element-type t)))

		(loop for i from 0 below (length clause-array) do
			(cond ((= (mod (+ i 1) 4) 0)
					(setf (aref clause-collection 1) (aref clause-array i))
					(setf (aref clause-collection 2) (to-clause-list 	(aref clause-array (+ i 1))
																		(aref clause-array (+ i 2))
																		(aref clause-array (+ i 3))))
					(setf (aref clause-collection 0) (clean-clause-list (aref clause-collection 0)											
																		(aref clause-collection 1)
																		(aref clause-collection 2)))
					(setf clause-list (aref clause-collection 0))
					(setf i (+ i 3)))
				(t  (setf (aref clause-collection 0) (to-clause-list 	(aref clause-array (+ i))
																		(aref clause-array (+ i 1))
																		(aref clause-array (+ i 2))))
					(setf i (+ i 2)))))
		(if clause-list ; If the clause-list instead, assumed ONE list to be accumulated in CLAUSE-COLLECTION
			clause-list
			(aref clause-collection 0))))
				
			
; Read a clause as presented after WHERE in an SQL Statement into a List-representation, that can be evaluated using EVAL etc.
; Should effectively also read all other types of clauses, and handles common operators, including NOT -> ID NOT 10
; Currently doesn't support math, appart from what could maybe be evaluated in 3 elements (2 + 2)
; Equally A func B could be used to call a function FUNC with parameters A and B. LIKE is thought to be implemented like this :: (ID LIKE 'a%') -> SEARCH 

(defun read-clause (stream)
	"Read a conditional (SQL) clause"
	(let (	(variable 	(make-array 0 	:element-type 'character		; Create an array to build a read variable or operator
										:fill-pointer 0
										:adjustable t))
			(atoms 		(make-array 0 	:element-type t					; Create an array to hold the built variable or operator
										:fill-pointer 0
										:adjustable t)))										
		(loop for c = (read-char stream nil nil)
			while c do
				(cond 
					((not (alphanumericp c))
						(when (char= c #\)) (return))
						(when (> (length variable) 0)
							(vector-push-extend (read-from-string variable) atoms))
						(setf (fill-pointer variable) 0)				; Reset the variable array
						(unless (typep c 'rwhitespaces)					; Unread the character to be used in the READ-NON-ALPHA function, unless it's a whitespace character
							(unread-char c stream)
							(vector-push-extend (read-non-alpha stream) atoms)))
					(t (vector-push-extend c variable))))
		(when (> (length variable) 0)
			(vector-push-extend (read-from-string variable) atoms))
		(clean-clause atoms)))