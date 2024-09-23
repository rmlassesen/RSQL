; R#UNE Structured Query Lisp - (:rascal)
; Common Lisp custom query language

; ONLY FOR TESTING - BETWEEN THIS
(defun ex () (sb-ext:exit))
(defun clear() (format t "~A[H~@*~A[J" #\escape))
(defun reload() (load "rsql.lsp"))

(defvar *mystream* nil)
(defun w() (setf *mystream* (open "test.ts" :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))))
(defun r() (setf *mystream* (open "test.ts" :direction :input :element-type '(unsigned-byte 8))))
(defun c() (close *mystream*))

; ONLY FOR TESTING - AND THIS

(defpackage :rsql
  (:use :cl))
(in-package :rsql)

; ONLY FOR TESTING - BETWEEN THIS
(defun ex () (sb-ext:exit))
(defun clear() (format t "~A[H~@*~A[J" #\escape))
(defun reload() (load "rsql.lsp"))

(defvar *mystream* nil)
(defun w() (setf *mystream* (open "test.ts" :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))))
(defun r() (setf *mystream* (open "test.ts" :direction :input :element-type '(unsigned-byte 8))))
(defun c() (close *mystream*))

; ONLY FOR TESTING - AND THIS

(defvar *data-dir* "Lists/")
(defvar *datatypes* 	 (make-hash-table :test 'equalp))
(defvar *datatypes-enum* (make-hash-table))


(setf (gethash "tinyint" 	*datatypes*) 0)		(setf (gethash 0  *datatypes-enum*) "tinyint")
(setf (gethash "binary" 	*datatypes*) 1)		(setf (gethash 1  *datatypes-enum*) "binary")
(setf (gethash "int" 		*datatypes*) 2)		(setf (gethash 2  *datatypes-enum*) "int")
(setf (gethash "integer" 	*datatypes*) 2)
(setf (gethash "unsint" 	*datatypes*) 3)		(setf (gethash 3  *datatypes-enum*) "unsint")
(setf (gethash "varchar"	*datatypes*) 4)		(setf (gethash 4  *datatypes-enum*) "varchar")
(setf (gethash "text"		*datatypes*) 4)	
(setf (gethash "float" 		*datatypes*) 5)		(setf (gethash 5  *datatypes-enum*) "float")
(setf (gethash "double"		*datatypes*) 6)		(setf (gethash 6  *datatypes-enum*) "double")
(setf (gethash "dec32" 		*datatypes*) 7)		(setf (gethash 7  *datatypes-enum*) "dec32")
(setf (gethash "dec64" 		*datatypes*) 8)		(setf (gethash 8  *datatypes-enum*) "decimal")
(setf (gethash "decimal" 	*datatypes*) 8)
(setf (gethash "bcrypt"		*datatypes*) 9)		(setf (gethash 9  *datatypes-enum*) "bcrypt")
(setf (gethash "scrypt"		*datatypes*) 10)	(setf (gethash 10 *datatypes-enum*) "scrypt")
(setf (gethash "argon2"		*datatypes*) 11)	(setf (gethash 11 *datatypes-enum*) "argon2")
(setf (gethash "date" 		*datatypes*) 12)	(setf (gethash 12 *datatypes-enum*) "date")
(setf (gethash "datetime"	*datatypes*) 13)	(setf (gethash 13 *datatypes-enum*) "datetime")
(setf (gethash "timestamp"	*datatypes*) 14)	(setf (gethash 14 *datatypes-enum*) "timestamp")
(setf (gethash "time" 		*datatypes*) 15)	(setf (gethash 15 *datatypes-enum*) "time")
(setf (gethash "year" 		*datatypes*) 16)	(setf (gethash 16 *datatypes-enum*) "year")

(ql:quickload "ironclad") ; Ironclad library for encryption and security
(declaim (optimize (speed 3) (debug 0)))

(load "declarations.lsp")
(load "helper-functions.lsp")
(load "datetime.lsp")
(load "floats.lsp")
(load "decimal.lsp")
(load "strings.lsp")
(load "password.lsp")
(load "read.lsp")
(load "write.lsp")
(load "jli.lsp")
(load "json.lsp")
(load "xml.lsp")
(load "csv.lsp")
(load "sql.lsp")