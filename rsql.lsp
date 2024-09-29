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


(ql:quickload "ironclad") ; Ironclad library for encryption and security
(declaim (optimize (speed 3) (debug 0)))

(load "datatypes.lsp")
(load "declarations.lsp")
(load "helper-functions.lsp")
(load "sql.lsp")
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

(init-databases)