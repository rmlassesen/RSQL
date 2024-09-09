; Helper functions for strings

(defun string-is-number (str)
	"Determines whether a string is entirely a number"
	(numberp (read-from-string str)))

(defun compare-string-values (operator x y)
	"Convert a simple operator to test on a string"
	(let (	(xn (read-from-string x))
			(yn (read-from-string y)))

		(cond
			((and (numberp xn) (numberp yn)) 							; If the strings are actually numbers, compare them as such
				(if (eq operator '!=) 	
					(not (= xn yn))
					(funcall operator xn yn)))
			((eq operator '=)	(string= x y))
			((eq operator '>)	(string> x y))
			((eq operator '<)	(string< x y))
			((eq operator '<=)	(string<= x y))
			((eq operator '>=)	(string>= x y))
			((eq operator '!=)	(not (string= x y)))
			(t nil))))
			
(defun split-string (str delimiter &optional (start 0) (end 0))
	"Returns an array of strings delimited by DELIMITER. DELIMITER must be a string"
	(let ((retarr (make-array 0	:element-type 'string
								:fill-pointer 0
								:adjustable t)))
		(when (= end 0 ) (setf end (length str)))
		(loop for npos = (search delimiter str :start2 start :end2 end)
			while npos do
				(when (and (> npos 0) (> (- npos start) 0))
					(vector-push-extend (subseq str	start npos) retarr))
				(setf start (+ 1 npos)))
		(when (> (length str) start) (vector-push-extend (subseq str start) retarr))
		retarr))			

(defun concatenate-list (lst)
	"Concatenate a list of strings to a single string separated by whitespaces"
	(reduce (lambda (a b)
		(concatenate 'string (format nil "~a" a) " " (format nil "~a" b)))
			lst))

(defun first-non-alphanumeric-char (str)
	"Finds first non-alphanumeric character in STR"
	(loop for i from 0 below (length str)
		for char = (char str i)
			if (not (alphanumericp char))
				return i))
		
(defun first-alphanumeric-char (str)
	"Finds first alphanumeric character in STR"
	(loop for i from 0 below (length str)
		for char = (char str i)
			if (alphanumericp char)
				return i))

(defun first-non-zero-in-string (str)
  "Find the first position of a character in string that is not #\0"
  (position-if (lambda (c) (not (eql c #\0))) str))

(defun or-find (orlist str &optional (start 0))
	"Returns a CONS (position delimiter) of the first occurence
	of any delimiter in STR"
	(let ((npos) (ret nil))
		(dolist (item orlist)
			(when (setf npos (search item str :start2 start :test #'char-equal))
				(if (car ret)
					(when (< npos (car ret))
						(setf ret (cons npos item)))
					(setf ret (cons npos item)))))
		ret))