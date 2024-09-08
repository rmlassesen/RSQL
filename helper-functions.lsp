; Helper functions needed in several pieces of the code 

(deftype rwhitespaces () '(member #\Space #\Tab #\Newline #\Return))
(deftype rcondoperators () '(member #\= #\< #\> #\!))

; String Helper-functions
(declaim (ftype (function (string) boolean) string-is-number))
(declaim (ftype (function (symbol string string) boolean) compare-string-values))
(declaim (ftype (function (string string &optional (integer 0) (integer 0)) array) split-string))
(declaim (ftype (function (list) string) concatenate-list))
(declaim (ftype (function (string) (or integer list)) first-non-alphanumeric-char))
(declaim (ftype (function (string) (or integer list)) first-alphanumeric-char))
(declaim (ftype (function (list string &optional (integer 0)) list) or-find))

; Stream Helper-functions
(declaim (ftype (function (string) stream) open-file-without-bom))
(declaim (ftype (function (string) string) import-as-string))
(declaim (ftype (function (stream)) pass-whitespaces))
(declaim (ftype (function (stream) string) read-single-quote-string))
(declaim (ftype (function (stream) t) read-non-alpha))

; Hash-table Helper-functions
(declaim (ftype (function (array) array) copy-jli))
(declaim (ftype (function (hash-table) hash-table) copy-hash-table))

; "SQL" Clause-helpers 
(declaim (ftype (function (t t t) list) to-clause-list))
(declaim (ftype (function (t t t) list) clean-clause-list))
(declaim (ftype (function (array) list) clean-clause))
(declaim (ftype (function (stream) list) read-clause))


(load "string-helpers.lsp")
(load "stream-helpers.lsp")
(load "hash-helpers.lsp")
(load "clause-helpers.lsp")