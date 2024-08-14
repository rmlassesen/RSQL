; R#UNE Structured Query Lisp - (:rascal)
; Common Lisp custom query language

;ONLY FOR TESTING - BETWEEN THIS
(defun ex () (sb-ext:exit))
(defun clear() (format t "~A[H~@*~A[J" #\escape))
(defun reload() (load "rsql.lsp"))

(defvar *mystream* nil)
(defun w() (setf *mystream* (open "test.ts" :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))))
(defun r() (setf *mystream* (open "test.ts" :direction :input :element-type '(unsigned-byte 8))))
(defun c() (close *mystream*))

;ONLY FOR TESTING - AND THIS


(ql:quickload "ironclad") ; Ironclad library for encryption and security

(load "floats.lsp")
(load "decimal.lsp")
(load "strings.lsp")
(load "password.lsp")
(load "read.lsp")
(load "write.lsp")