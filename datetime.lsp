(in-package :rsql)
; Datetime formats
; Note that string formats are converted by an integers placement in a string, and is not separated by delimiters;
; Thus, you can use any separator (of one character length) in the date-time formats

; Timestamp
; Using Modified Unix Epoc Timestamp
; Seconds since 00/00/0000 saved as a signed 64-bit integer

; Unix
; Using Unix Epoc Timestamp
; Seconds since 01/01/1970 saved as a signed 64-bit integer

; Date
; ISO 8601 format (date part) YYYY-MM-DD
; Saved in 24 bits; 6-bits for DD, 6-bits for MM and 12-bits for YYYY

; Datetime
; ISO 8601 format YYYY-MM-DDTHH:mm:ss.ms format (with miliseconds) 
; Miliseconds should default to zero, if :ms is omitted
; Saved in 48 bits; 6 bits for DD,  6-bits for MM and 12-bits for YYYY 
; then 5 bits for HH, 6 bits for mm, 6 bits for ss and 7 bits for ms, allowing for a 0-99ms range

(defun datetime-from-timestamp (timestamp)
	"Return as an array of arrays (DATETIME) with 48 bits in total"
	(let ((date-arr (make-array 7 :element-type t)))
		(multiple-value-bind (ss m hh dd mm yyyy)
			(decode-universal-time timestamp)
			(setf (aref date-arr 0) (int-to-bit yyyy))
			(setf (aref date-arr 1) (int-to-bit   mm))
			(setf (aref date-arr 2) (int-to-bit   dd))
			(setf (aref date-arr 3) (int-to-bit   hh))
			(setf (aref date-arr 4) (int-to-bit    m))
			(setf (aref date-arr 5) (int-to-bit   ss))
			(setf (aref date-arr 6) (int-to-bit    0)))
		date-arr))

(defun datetime-from-string (datetime-string)
	"Convert a string formated as YYYY-MM-DDTHH:mm:ss.ms into an array of arrays of bits"
	(let ((date-arr (make-array 7 :element-type t)))
		(when (>
				(setf (aref date-arr 0) (parse-integer datetime-string :start 0 :end 4))
				4096) (error "The future may only be imagined until 4096"))
		(when (> 
				(setf (aref date-arr 1) (parse-integer datetime-string :start 5 :end 7))
				12) (error "A year can't have more than 12 months"))
		(when (> 
				(setf (aref date-arr 2) (parse-integer datetime-string :start 8 :end 10))
				31) (error "A month can't have more than 31 days"))
		(when (>
				(setf (aref date-arr 3) (parse-integer datetime-string :start 11 :end 13))
				24) (error "A day only has 24 hours"))
		(when (> 
				(setf (aref date-arr 4) (parse-integer datetime-string :start 14 :end 16))
				59) (error "An hour only holds 60 minutes"))
		(when (> 
				(setf (aref date-arr 5) (parse-integer datetime-string :start 17 :end 19))
				59) (error "A minute only holds 60 seconds"))
		(if (> (length datetime-string) 20)
			(when (> 
					(setf (aref date-arr 6) (parse-integer datetime-string :start 20))
					99) (error "Format only supports miliseconds from 0-99"))
			(setf (aref date-arr 6) 0))
		(map-into date-arr #'int-to-bit date-arr)
		date-arr))

(defun date-from-string (date-string)
	"Convert a string formated as YYYY-MM-DD into an array of arrays of bits" 
	(let ((date-arr (make-array 3 :element-type t)))
		(when (>
				(setf (aref date-arr 0) (parse-integer date-string :start 0 :end 4))
				4096) (error "The future may only be imagined until 4096"))
		(when (> 
				(setf (aref date-arr 1) (parse-integer date-string :start 5 :end 7))
				12) (error "A year can't have more than 12 months"))
		(when (> 
				(setf (aref date-arr 2) (parse-integer date-string :start 6))
				31) (error "A month can't have more than 31 days"))
		
		(map-into date-arr #'int-to-bit date-arr)
		date-arr))

(defun now () 
	(datetime-from-timestamp (get-universal-time)))
		
(defun date-to-string (dd mm yyyy)
	(format nil "~a/~a/~a"	dd mm yyyy))

(defun date-to-iso-string (dd mm yyyy)
	(format nil "~a-~a-~a"	yyyy mm dd))
	
(defun us-date-to-string (dd mm yyyy)
	(format nil "~a/~a/~a"	mm dd yyyy))
	
(defun datetime-to-string (dd mm yyyy hh m ss ms)
	(if (> 0 ms)
		(format nil "~a/~a/~a ~a:~a:~a.~a"	dd mm yyyy hh m ss ms)
		(format nil "~a/~a/~a ~a:~a:~a"	dd mm yyyy hh m ss)))

(defun datetime-to-iso-string (dd mm yyyy hh m ss ms)
	(if (> 0 ms)
		(format nil "~a-~a-~aT~a:~a:~a.~a"	yyyy mm dd hh m ss ms)
		(format nil "~a-~a-~aT~a:~a:~a"	yyyy mm dd hh m ss)))

(defun datetime-to-iso-string-no-T (dd mm yyyy hh m ss ms)
	(if (> 0 ms)
		(format nil "~a-~a-~a ~a:~a:~a.~a"	yyyy mm dd hh m ss ms)
		(format nil "~a-~a-~a ~a:~a:~a"	yyyy mm dd hh m ss)))
	
(defun us-datetime-to-string (dd mm yyyy hh m ss ms)
	(if (> 0 ms)
		(format nil "~a/~a/~a ~a:~a:~a.~a"	mm dd yyyy hh m ss ms)
		(format nil "~a/~a/~a ~a:~a:~a"	mm dd yyyy hh m ss)))
	
(defun date-from-epoch (timestamp)
	(multiple-value-bind (ss m hh dd mm yyyy)
		(decode-universal-time timestamp)
		(datetime-to-string dd mm yyyy hh m ss 0)))

(defun iso-date-from-epoch (timestamp)
	(multiple-value-bind (ss m hh dd mm yyyy)
		(decode-universal-time timestamp)
		(datetime-to-iso-string dd mm yyyy hh m ss 0)))

(defun no-t-iso-from-epoch (timestamp)
	(multiple-value-bind (ss m hh dd mm yyyy)
		(decode-universal-time timestamp)
		(datetime-to-iso-string-no-T dd mm yyyy hh m ss 0)))