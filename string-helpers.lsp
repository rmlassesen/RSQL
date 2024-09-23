(in-package :rsql)
; Helper functions for strings

(defun string-is-number (str)
	"Determines whether a string is entirely a number"
	(numberp (read-from-string str)))

(defun to-keyword (str)	
	(read-from-string
		(format nil ":~a" str)))
		
(defun from-keyword (kw)
	(format nil "~a" kw))

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

(defun first-non-alphanumeric-char (str &optional (start 0) (end (length str)))
	"Finds first non-alphanumeric character in STR"
	(position-if (lambda (c) (not (alphanumericp c))) 
					str 
					:start start 
					:end end))

(defun first-alphanumeric-char (str &optional (start 0) (end (length str)))
	"Finds first alphanumeric character in STR"
	(position-if (lambda (c) (alphanumericp c))
					str 
					:start start 
					:end end))
					
(defun first-space-or-paren (str &optional (start 0) (end (length str)))
	"Finds first alphanumeric character in STR"
	(position-if (lambda (c) (or (eql c #\Space) (eql c #\()))
					str 
					:start start 
					:end end))

(defun first-non-zero-in-string (str &optional (start 0) (end (length str)))
  "Find the first position of a character in string that is not #\0"
  (position-if (lambda (c) (not (eql c #\0)))
					str
					:start start
					:end end))

(defun remove-end-space (str)
	"Checks if the last charecter is a space and removes it"
	(when (eql (aref str (- (length str) 1)) #\Space)
		(setf str (subseq str 0 (- (length str) 2))))
	str)
	
(defun remove-start-space (str)
	"Checks if the first charecter is a space and removes it"
	(when (eql (aref str 0) #\Space)
		(setf str (subseq str 1)))
	str)
	
(defun remove-spaces (str)
	(remove-if #'(lambda (c) (char= c #\Space)) str))

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
		
(defun list-to-array (lst)
	"Convert a list to an array"
	(let ((arr (make-array (length lst) :element-type t)) (i 0))
		(dolist (obj lst)
			(setf (aref arr i) obj)
			(incf i))
		arr))