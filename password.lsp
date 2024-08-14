; Password data-type
; Stores and acceses paswords as a hashed password, salt and method as a single entry
; Using Ironclad library's salt and KDF functionality (ironclad::xxxx)
; All salts are 16 bytes, and salted passwords are hashed with argon2, bcrypt or scrypt

(defvar *scrypt* (ironclad::make-kdf :scrypt-kdf))
(defvar *argon2* (ironclad::make-kdf :argon2i))
(defvar *bcrypt* (ironclad::make-kdf :bcrypt))

(defun make-8bit-char-array (str)
	"Make an array of 8bit chars from string"
	(let ( (char-array (make-array (length str) :element-type '(unsigned-byte 8))))
			
		(loop for ch across str for i from 0 do 
			(setf (aref char-array i) (char-code ch)))
		char-array))

(defun make-salt ()
	"Generate a random salt of 16 bytes"
	(ironclad::make-random-salt 16))
	
(defun hash-argon2-password (password salt)
	"Salt a password, and hash it using argon2i"
	(ironclad::derive-key *argon2* (make-8bit-char-array password) salt 1 64))
	
(defun hash-bcrypt-password (password salt)
	"Salt a password, and hash it using bcrypt"
	(ironclad::derive-key *bcrypt* (make-8bit-char-array password) salt 32 24))

(defun hash-scrypt-password (password salt)
	"Salt a password, and hash it using scrypt"
	(ironclad::derive-key *scrypt* (make-8bit-char-array password) salt 32 24))	