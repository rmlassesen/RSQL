(in-package :rsql)
; SQL Function declarations

; SQL Readtable
(defvar *SQL-readtable* (copy-readtable nil))  ; Create a global SQL readtable
(defvar defaultreadtable (copy-readtable nil)) ; Create a standard readtalbe for reset - without any special characters in the variable name to be caught by the reader


; Database / Schema handling
(declaim (ftype (function (stream)) create-database))
(declaim (ftype (function (stream)) use-db))
(declaim (ftype (function (symbol symbol) t) make-tables))
(declaim (ftype (function (symbol) t) make-schema))
(declaim (ftype (function ()) init-databases))


; Table Handling
(declaim (ftype (function (stream) t) make-table-form))
(declaim (ftype (function (stream) t) create-table))

; SQL Insert handling
(declaim (ftype (function (stream) list)  make-insert-form))
(declaim (ftype (function (stream)) insert-into))

; SQL Select handling

; SQL Parser
(declaim (ftype (function (stream)) sql-parse))
(declaim (ftype (function (stream)) sql-parser))

; Load files containing declared SQL functions
(load "sql-readtable.lsp")
(load "sql-databases.lsp")
(load "sql-tables.lsp")
(load "sql-insert.lsp")
(load "sql-clause.lsp")
(load "sql-select.lsp")
(load "sql-parser.lsp")