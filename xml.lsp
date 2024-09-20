; XML as a datatype
; Creation of JSON-like-objects in Lisp, to capture the structure of XML
; Possibility of importing these from .XML files

; JSON-like-object - JSON Lisp Interpretation JLI (pronunciation: Jelly EN-UK)
; JavaScript Object Notation Lisp Interpretation

; JLI takes form as a simple array of hash-tables, representing each JSON {},[] encapsulation with an array of hash-tables
; In XML {} and [] are equivalent to <record> and <dataset>

(defun read-tag-name (stream)
	"Extract name of tag as :KEYWORD"
	(read-char stream)												; Remove < from stream
	(let ((start (file-position stream)) (npos 0) (tag))
		(when (peek-char #\> stream nil nil)
			(setf npos (file-position stream))
			(file-position stream start))
	(setf tag 
		(case (peek-char nil stream nil nil) 						; If the tag indicates PROLOG, COMMENT etc. return appropriate tag
			(#\?	(case (read stream nil 'eof)
						('?xml	:RXPROLOG)
						(t :ERROR)))
			(#\!	(case (read stream nil 'eof)
						('!--	:RXCOMMENT)
						(t :ERROR)))
			(#\/	(read-char stream)								; Remove peeked char #\/
					(read-from-string 
						(concatenate 'string ":END"
							(loop for _ from start to (- npos 2)	; NPOS is the read-position, minus 1 for >, minus 1 for #\/
								collect (read-char stream)))))
			(nil :ERROR)
			(t 		(read-from-string 
						(concatenate 'string ":"
							(loop for _ from start to (- npos 1)	; NPOS is the read-position, minus 1 for >
								collect (read-char stream)))))))
	(read-char stream)												; Remove > from stream
	tag))


(defun read-xml-content (stream)
	"Extract content between two tags (from > to <)"
	(let ((start (file-position stream)) (npos 0))
		(when (peek-char #\< stream nil nil)
			(setf npos (file-position stream))
			(file-position stream start))
		(concatenate 'string
			(loop for _ from start to (- npos 1) 					; NPOS is the read-position, minus 1 for <
				collect (read-char stream)))))

(defun read-xml-prolog (stream)
	"Return the XML prolog as string"
	(let ((start (file-position stream)) (npos 0) (prolog))
		(when (peek-char #\> stream nil nil)
			(setf npos (file-position stream))
			(file-position stream start))
		(setf prolog 
			(concatenate 'string
				(loop for _ from start to (- npos 2) 				; NPOS is the read-position, minus 1 for ?, minus 1 for > (PROLOG END: ?>)
					collect (read-char stream))))
		(file-position stream (+ npos 1))							; Skips ?>
		prolog))

(defun read-xml-record (stream)
	"Return a JLI-object from XML-Dataset element"
	(let ((jli-hash (make-hash-table)))
		(loop for key = (read-tag-name stream)
			while (not (eq key :ENDRECORD)) do
				(setf (gethash key jli-hash) (read-xml-content stream))
				(unless (eq (read-tag-name stream)
							(read-from-string 
								(format nil ":END~A" key)))
								(error "Unmatched endtag for ~A" key)))
	jli-hash))

(defun xml-to-jli (stream)
	"Read XML-file STREAM and return a JLI-object"
	(let ((jli (make-array 1 	:element-type 't 
								:fill-pointer 0
								:adjustable t)))
	
		(loop for c = (peek-char nil stream nil nil)
			while c do
				(if (char= c #\<)
					(case (read-tag-name stream)
						((:DATASET)		(vector-push-extend (xml-to-jli stream) jli)) 
						((:RECORD)		(vector-push-extend (read-xml-record stream) jli))
						((:RXPROLOG)	(read-xml-prolog stream))
						((:RXCOMMENT)	(skip-xml-comment stream))
						((:ENDDATASET)	))
					(read-char stream nil nil)))
		(if (= (length jli) 1)
			(aref jli 0)											; If the array contains only ONE dataset, return that one ONLY
			jli)))													; Else return entire array of datasets (datasets are arrays/JLI-objects)
				
			
				
(defun import-xml-from-file (xml-file-path)
	"Open XML-file and return it as a JLI-object"
	(let* (	(stream (open-file-without-bom xml-file-path))
			(jli (xml-to-jli stream)))
		(close stream)
		jli))