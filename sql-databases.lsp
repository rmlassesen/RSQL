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
			(write-line (string-downcase (string db-name)) stream))))				; Write database name to schemas.rtb
			
(defun test-create ()
	(with-input-from-string (test-stream "create database test_db")
	(sql-parse test-stream)))

(defun use-db (stream)
	(let ((db-name (read stream nil nil)))
		(unless (gethash db-name *schemas*)
			(error "Database ~a does not exist" db-name))
		(setf *in-db* db-name)))

(defun make-tables (schema-name table-name)
	(let (	(table-form (read-table-form schema-name table-name))
			(data-size ) (keys '()) (keypair "") (keytypes '()))
		(maphash (lambda (k v)					; Create a list of FIELD-NAME and ROWNUM from TABLES:PRIMARY
			(push (cons k v) keys))
			(primary table-form))
		(setf keys (sort keys (lambda (a b)		; Sort the keys by ROWNUM
						(< (cdr a) (cdr b)))))
		(dolist (k keys)
			(push keytypes (datatype (gethash k (fields table-form))))
			(setf keypair (concatenate 
							'string 
							keypair
							(string (car k))
							"_")))
							
		(setf data-size (read-data-size			; Read data-size (returns CONS of number of files and number of rows)
							schema-name
							table-name
							(reverse keypair)
							keytypes))
		(setf (files table-form) 	(car data-size))
		(setf (rowcount table-form) (cdr data-size))
		table-form))
		

			
	
(defun make-schema (schema-name)
	(let ((sch (make-instance 'schema)))
		(with-open-file 	
			(stream (concatenate 
						'string
						*data-dir*  
						(string-downcase (string schema-name)) 	; Only because lower-case file-names are pretier
						"/tables.rtb" )
					:if-does-not-exist :create 
					:direction :input)						
		(loop for l = (read stream nil nil)
			while l do
				(setf (gethash l (tables sch)) (make-tables schema-name l)))
		sch)))

(defun init-databases ()	 
	(with-open-file 
		(stream (concatenate 'string
							*data-dir* 
							"/schemas.rtb")
						:if-does-not-exist :create 
						:direction :input)
		(loop for l = (read stream nil nil)
			while l do
				(setf (gethash l *schemas*) (make-schema l)))))