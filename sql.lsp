(in-package :rsql)
; SQL Function declarations

; SQL Readtable
(defvar *SQL-readtable* (copy-readtable nil))  ; Create a global SQL readtable

; Table Handling
(declaim (ftype (function (string) list) make-table-form))

; SQL Insert handling
(declaim (ftype (function (stream) list)  make-insert-form))

; Load files containing declared SQL functions
(load "sql-readtable.lsp")
(load "sql-tables.lsp")
(load "sql-insert.lsp")