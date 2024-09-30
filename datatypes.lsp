; Datatype handling
(in-package :rsql)

; Data-types: 
; -1	Index Integer (signed 64-bit big endian)			(Can't be null - set for INTS that are PRIMARY KEYS)		
; 0		Bytes(unsigned 8-bit)								(BYTES can't be NULL, but 00000000 or 11111111 is okay)
; 1		Tinyinte (signed 8-bit)								(NULL is represented with 8 1-ones (2^8)
; 2		Small integer (signed 16-bit)						(NULL is represented with 16 1-ones (2^16))
; 3		Medium integer (signed 24-bit)						(NULL is represented with 16 1-ones (2^24))
; 4		Integer (signed 32-bit)								(NULL is represented with 32 1-ones (2^32))
; 5		Positive Only Integers(unsigned 32-bit)				(NULL is represented with 32 1-ones (2^32))
; 6		Big Integers(signed 64-bit)							(NULL is represented with 64 1-ones (2^64))
; 7		Char-sequences/String(UTF-8)						(NULL is represented with character code 0: #\Nul)
; 8		IEEE 754 floating point 32-bit representation		(NULL is represented with 32 1-ones (2^32))
; 9		IEEE 754 floating point 64-bit representation		(NULL is represented with 64 1-ones (2^64))
; 10	Custom signed decimal 32-bit representation			(NULL is represented with 32 1-ones (2^32))
; 11	Custom signed decimal 64-bit representation			(NULL is represented with 64 1-ones (2^64))
; 12	Password hashed with bcrypt							(PASSWORDS cannot be NULL)
; 13	Password hashed with scrypt							(PASSWORDS cannot be NULL)
; 14	Password hashed with argon2i						(PASSWORDS cannot be NULL)
; 15	Date												(NULL is represented by 0000-00-00 - effectively all bytes are zero)
; 16	Datetime											(NULL is represented by 0000-00-00T00:00:00.00  - effectively all bytes are zero)
; 17	Timestamp (unsigned 40-bit integer)					(NULL is represented by 40 1-ones (2^40))
; 18	Time												(NULL is represented by 00:00:00.00  - effectively all bytes are zero)
; 19	Year												(NULL is 0000  - effectively all bytes are zero)


(defvar *datatypes* 	 (make-hash-table :test 'equalp))
(defvar *datatypes-enum* (make-hash-table))
(defvar *null-values*	 (make-hash-table))


(setf (gethash 'index 		*datatypes*) -1)	(setf (gethash -1 *datatypes-enum*) 'index)			(setf (gethash -1  *null-values*) 'error)
(setf (gethash 'binary	 	*datatypes*) 0)		(setf (gethash 0  *datatypes-enum*) 'binary)		(setf (gethash 0   *null-values*) 'error)
(setf (gethash 'tinyint	 	*datatypes*) 1)		(setf (gethash 1  *datatypes-enum*) 'tinyint)		(setf (gethash 1   *null-values*) (- 1 (expt 2  8)))
(setf (gethash 'smallint 	*datatypes*) 2)		(setf (gethash 2  *datatypes-enum*) 'smallint)		(setf (gethash 2   *null-values*) (- 1 (expt 2 16)))
(setf (gethash 'mediumint 	*datatypes*) 3)		(setf (gethash 3  *datatypes-enum*) 'mediumint)		(setf (gethash 3   *null-values*) (- 1 (expt 2 24)))
(setf (gethash 'int 		*datatypes*) 4)		(setf (gethash 4  *datatypes-enum*) 'integer)		(setf (gethash 4   *null-values*) (- 1 (expt 2 32)))
(setf (gethash 'integer 	*datatypes*) 4)
(setf (gethash 'unsint	 	*datatypes*) 5)		(setf (gethash 5  *datatypes-enum*) 'unsint)		(setf (gethash 5   *null-values*) (- (expt 2 32) 1))
(setf (gethash 'bigint	 	*datatypes*) 6)		(setf (gethash 6  *datatypes-enum*) 'big)			(setf (gethash 6   *null-values*) (- (expt 2 64) 1))
(setf (gethash 'char		*datatypes*) 7)		(setf (gethash 7  *datatypes-enum*) 'text)			(setf (gethash 7   *null-values*) (string #\Nul))
(setf (gethash 'varchar		*datatypes*) 7)
(setf (gethash 'text		*datatypes*) 7)	
(setf (gethash 'tinytext	*datatypes*) 7)	
(setf (gethash 'mediumtext	*datatypes*) 7)	
(setf (gethash 'longtext	*datatypes*) 7)		
(setf (gethash 'float 		*datatypes*) 8)		(setf (gethash 8  *datatypes-enum*) 'float)			(setf (gethash 8   *null-values*) (- 1 (expt 2 32)))
(setf (gethash 'double		*datatypes*) 9)		(setf (gethash 9  *datatypes-enum*) 'double)		(setf (gethash 9   *null-values*) (- 1 (expt 2 64)))
(setf (gethash 'dec32 		*datatypes*) 10)	(setf (gethash 10 *datatypes-enum*) 'dec32)			(setf (gethash 10  *null-values*) (- 1 (expt 2 32)))
(setf (gethash 'dec64 		*datatypes*) 11)	(setf (gethash 11 *datatypes-enum*) 'decimal)		(setf (gethash 11  *null-values*) (- 1 (expt 2 64)))
(setf (gethash 'decimal 	*datatypes*) 11)
(setf (gethash 'bcrypt		*datatypes*) 12)	(setf (gethash 12 *datatypes-enum*) 'bcrypt)		(setf (gethash 12  *null-values*) 'error)
(setf (gethash 'scrypt		*datatypes*) 13)	(setf (gethash 13 *datatypes-enum*) 'scrypt)		(setf (gethash 13  *null-values*) 'error)
(setf (gethash 'argon2		*datatypes*) 14)	(setf (gethash 14 *datatypes-enum*) 'argon2)		(setf (gethash 14  *null-values*) 'error)
(setf (gethash 'date 		*datatypes*) 15)	(setf (gethash 15 *datatypes-enum*) 'date)			(setf (gethash 15  *null-values*) "0000-00-00")
(setf (gethash 'datetime	*datatypes*) 16)	(setf (gethash 16 *datatypes-enum*) 'datetime)		(setf (gethash 16  *null-values*) "0000-00-00T00:00:00.00")
(setf (gethash 'timestamp	*datatypes*) 17)	(setf (gethash 17 *datatypes-enum*) 'timestamp)		(setf (gethash 17  *null-values*) (- (expt 2 40) 1))
(setf (gethash 'time 		*datatypes*) 18)	(setf (gethash 18 *datatypes-enum*) 'time)			(setf (gethash 18  *null-values*) "00:00:00.00")
(setf (gethash 'year 		*datatypes*) 19)	(setf (gethash 19 *datatypes-enum*) 'year)			(setf (gethash 19  *null-values*) "0000")

(defun checktype (data datatype)
	(when (eql data 'null)
		(return-from checktype t))
	(case datatype
		(0 (integerp data))
		(1 (integerp data))
		(2 (integerp data))
		(3 (integerp data))
		(4 (stringp data))
		(5 (numberp data))
		(6 (numberp data))
		(7 (numberp data))
		(8 (numberp data))
		(9 (stringp data))
		(10 (stringp data))
		(11 (stringp data))
		(12 (and (stringp data)
				(= (length data) 10)))		; Length 10 for chars in YYYY-MM-DD
		(13 (and (stringp data)
				(or (= (length data) 19)	; Length 19 for chars in YYYY-MM-DDTHH:mm:ss
					(= (length data) 22))))	; Length 22 for chars in YYYY-MM-DDTHH:mm:ss.ms
		(14 (integerp data))
		(15 (and (stringp data)
				(or (= (length data) 8)		; Length 8 for chars in HH:mm:ss
					(= (length data) 11))))	; Length 11 for chars in HH:mm:ss.ms
		(16 (and (stringp data)
				(= (length data) 4)))))		; Length 4 for chars in YYYY