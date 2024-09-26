(in-package :rsql)
; SQL Function declarations

; SQL Readtable
(defvar *SQL-readtable* (copy-readtable nil))  ; Create a global SQL readtable
(defvar defaultreadtable (copy-readtable nil)) ; Create a standard readtalbe for reset - without any special characters in the variable name to be caught by the reader

; SQL Parser

; Database / Schema handling

; Table Handling
(declaim (ftype (function (stream) t) make-table-form))

; SQL Insert handling
(declaim (ftype (function (stream) list)  make-insert-form))

; SQL Select handling

; Load files containing declared SQL functions
(load "sql-readtable.lsp")
(load "sql-databases.lsp")
(load "sql-tables.lsp")
(load "sql-insert.lsp")
(load "sql-clause.lsp")
(load "sql-select.lsp")
(load "sql-parser.lsp")