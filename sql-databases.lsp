(in-package :rsql)

(defvar *in-db* 'mock_db)
(defvar *schemas* (make-hash-table))

(defclass field ()
	((rownum			:type integer		; Number holds the ROW index number. E.g. ID in ID, USR, EMAIL is 0 (USR is 1 and EMAIL is 3).
						:accessor rownum
						:initarg :rownum)
	 (datatype 			:type integer		; TYPE specifies the datatype based on the RSQL  data-type enumerations
						:accessor datatype)
	 (primary 			:type keyword 		; PRIMARY specifies with keywords :TRUE or :FALSE, whether the field is a PRIMARY KEY/Index
						:initform :FALSE
						:accessor primary)
	 (unique 			:type keyword 		; PRIMARY specifies with keywords :TRUE or :FALSE, whether the field is a UNIQUE INDEX
						:initform :FALSE
						:accessor unique)
	 (auto_increment 	:type keyword		; AUTO_INCREMENT specifies with keywords :TRUE or :FALSE if the data should auto-increment (integers only)
						:initform :FALSE
						:accessor auto_increment)
	 (nul 				:type keyword		; NULL specifies with keywords :TRUE or :FALSE if value can be NULL (default :TRUE - set to :FALSE with NOT_NULL)
						:initform :TRUE
						:accessor nul)))
(defclass table ()
	((name		:type symbol					; NAME contains the table name as a 'symbol
				:accessor name)
	 (fields 	:type hash-table				; FIELDS contain a hash of a FIELD class indexed by FIELD NAME
				:initform (make-hash-table)
				:accessor fields)
	 (primary	:type hash-table 				; PRIMARY contains a hash of the PRIMARY KEY and its ROWNUM
				:initform (make-hash-table)		;  - usually only one, but can be COLUMN PAIRS
				:accessor primary)
	 (indexes	:type hash-table 				; INDEXES contains a hash of an INDEXES field and its ROWNUM
				:initform (make-hash-table)
				:accessor indexes)				
	 (files		:type integer 	 :initform 0	; Number of files the table is split into
				:accessor files)
	 (rowcount	:type integer	 :initform 0	; Number of ROWS the table contains
				:accessor rowcount)))

(defclass schema ()
	((tables :type hash-table :accessor tables :initform (make-hash-table))))

(defun create-database (stream)
	(let ((db-name (read stream nil nil)))
		(unless db-name
			(error "Statement is malformed: No database specified"))
		(when (gethash db-name *schemas*)
			(error "Database ~a already exists" db-name))
		(setf (gethash db-name *schemas*)
			(make-instance 'schema))
		(with-open-file (stream (concatenate 'string *data-dir* "/schemas.rtb")
					:direction :output
					:if-exists :append
					:if-does-not-exist :create)
			(write-line table-name stream))))				; Write database name to schemas.rtb
			

(defun use-db (stream)
	(let ((db-name (read stream nil nil)))
		(unless (gethash db-name *schemas*)
			(error "Database ~a does not exist" db-name))
		(setf *in-db* db-name)))

(defun make-tables (schema-name table-name)
	(let* (	(table-form (read-table-form schema-name table-name))
			(data-size (read-data-size (second table-form)))
			(tbl (make-instance 'table 
								:files (car data-size)
								:rowcount (cdr data-size))))
		(loop for i from 0 below (length (first table-form)) do					; Instantiate the fields with number and datatype
			(setf (gethash (car (aref (first table-form) i)) (fields tbl))		; TODO: expand on READ-TABLE-FORM to return UNIQUE, PRIMARY, AUTO-INCREMENT and NULL
				(make-instance 'field 	:number i
										:datatype (cdr (aref (first table-form) i))
										)))
		(loop for idx across (second table-form) do
			(setf (gethash idx (indexes tbl)) 0))))
			
			
	
(defun make-schema (schema-name)
	(let ((sch (make-instance 'schema)))
		(with-open-file 	
			(stream (concatenate 
						'string
						*data-dir*  
						(downcase (string schema-name)) 	; Only because lower-case file-names are pretier
						"/tables.rtb" )
					:if-does-not-exist :create 
					:direction :input)
										
		(loop for l = (read stream nil nil)
			while l do
				(setf (gethash l (tables sch)) (make-tables schema-name l)))
		sch)))
	 
(with-open-file 
	(stream (concatenate 'string
						*data-dir* 
						"/schemas.rtb")
					:if-does-not-exist :create 
					:direction :input)
	(loop for l = (read stream nil nil)
		while l do
			(setf (gethash l *schemas*) (make-schema l))))