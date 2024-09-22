(in-package :rsql)
; Return value from table row

(defun get_table (table_name)
	(let (	(tbl)
			(in (open (concatenate 'string "Lists/" table_name ".tbl") :if-does-not-exist nil)))
		(when in
			(setf tbl (list (read in) (cons (read in) (read in))))
			(close in))
		tbl
	)
) 

(format t "~a ~%" (get_table "produkter"))

(defun read_rows (item_numbers)
"Read item numbers '(x y z ...) from row and return as list"
	(let ( (in (open "Lists/produkter1.row" :if-does-not-exist nil)) (n 0) (i 1) (return_list '()) (temp_list '()))
		(setf n (car(read in))) ;Read the first row length into N
		
		(when in
			(dolist (num item_numbers)
				(loop 
					(if (= num i)
						(return (dotimes (_ n)
									(setf temp_list (append temp_list (list (read-char in))))))
						(dotimes (_ n) (read-char in)))
					(incf i)
					(setf n (car(read in))) ;Read row length from new position
				)
				(incf i)
				(setf n (car(read in))) ;Read row length from new position
				(setf return_list (append return_list (list (list (coerce temp_list 'string)))))
				(setf temp_list '())
			)
			(close in))
		return_list
	)
)

(format t "~a ~%" (read_rows (list 2 4)))