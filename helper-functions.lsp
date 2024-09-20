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
(declaim (ftype (function (string) (or integer list)) first-non-zero-in-string))
(declaim (ftype (function (list string &optional (integer 0)) list) or-find))

; Stream Helper-functions
(declaim (ftype (function (string) stream) open-file-without-bom))
(declaim (ftype (function (string) string) import-as-string))
(declaim (ftype (function (stream)) pass-whitespaces))
(declaim (ftype (function (stream) string) read-single-quote-string))
(declaim (ftype (function (stream) t) read-non-alpha))

; Binary Helper functions
(declaim (ftype (function (integer) array) int-to-bit))
(declaim (ftype (function (integer) array) byte-to-bits))
(declaim (ftype (function (array &optional (integer *) (integer *)) integer) bit-to-int))

; Hash-table Helper-functions
(declaim (ftype (function (array) array) copy-jli))
(declaim (ftype (function (hash-table) hash-table) copy-hash-table))

; "SQL" Clause-helpers 
(declaim (ftype (function (t t t) list) to-clause-list))
(declaim (ftype (function (t t t) list) clean-clause-list))
(declaim (ftype (function (array) list) clean-clause))
(declaim (ftype (function (stream) list) read-clause))

; Write Helpers
(declaim (ftype (function (stream integer)) write-8bit-value))
(declaim (ftype (function (stream integer)) write-16bit-value))
(declaim (ftype (function (stream integer)) write-32bit-value))
(declaim (ftype (function (stream integer integer)) write-30bit-typevalue))
(declaim (ftype (function (stream integer)) write-64bit-value))
(declaim (ftype (function (stream integer)) write-64bit-big-endian))
(declaim (ftype (function (stream integer)) write-signed-8bit-value))
(declaim (ftype (function (stream integer)) write-signed-64bit-value))
(declaim (ftype (function (stream integer)) write-64bit-big-endian))
(declaim (ftype (function (stream array)) write-8bit-charseq))
(declaim (ftype (function (stream array)) write-16bit-charseq))
(declaim (ftype (function (stream array)) write-custom-bit-array))
(declaim (ftype (function (stream array))write-utf-8-charseq))
(declaim (ftype (function (stream number)) write-32-bit-float))
(declaim (ftype (function (stream number)) write-64-bit-float))
(declaim (ftype (function (stream number)) write-32-bit-decimal))
(declaim (ftype (function (stream number)) write-64-bit-decimal))
(declaim (ftype (function (stream string)) write-argon2-password))
(declaim (ftype (function (stream string)) write-scrypt-password))
(declaim (ftype (function (stream string)) write-bcrypt-password))
(declaim (ftype (function (stream string)) write-date))
(declaim (ftype (function (stream string)) write-datetime))


; Read Helpers
(declaim (ftype (function (stream) integer) read-8bit-value))
(declaim (ftype (function (stream integer) array) read-bytes-to-bit-array))
(declaim (ftype (function (stream) integer) read-16bit-value))
(declaim (ftype (function (stream) integer) read-64bit-value))
(declaim (ftype (function (stream) integer) read-64bit-big-endian))
(declaim (ftype (function (stream integer) string) utf-8-to-string))
(declaim (ftype (function (stream) integer) read-signed-byte-8))
(declaim (ftype (function (stream) integer) read-signed-64bit-value))
(declaim (ftype (function (stream integer) string) read-16bit-charseq))
(declaim (ftype (function (stream integer) string) read-16bit-charseq))
(declaim (ftype (function (stream) number) read-32-bit-decimal))
(declaim (ftype (function (stream) number) read-64-bit-decimal))
(declaim (ftype (function (stream) float) read-32-bit-float))
(declaim (ftype (function (stream) float) read-64-bit-float))
(declaim (ftype (function (stream string) t) read-argon2-password))
(declaim (ftype (function (stream string) t) read-scrypt-password))
(declaim (ftype (function (stream string) t) read-bcrypt-password))
(declaim (ftype (function (stream) string) read-date))
(declaim (ftype (function (stream) string) read-datetime))

; Load files containing declared helpers functions
(load "string-helpers.lsp")
(load "stream-helpers.lsp")
(load "binary-helpers.lsp")
(load "hash-helpers.lsp")
(load "clause-helpers.lsp")
(load "write-helpers.lsp")
(load "read-helpers.lsp")