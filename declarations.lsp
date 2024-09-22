(in-package :rsql)
; Declarations of 'primary' functions

; Strings
(declaim (ftype (function () array) unknown-char))
(declaim (ftype (function (integer) array) 1-byte-utf-8))
(declaim (ftype (function (integer) array) 2-byte-utf-8))
(declaim (ftype (function (integer) array) 3-byte-utf-8))
(declaim (ftype (function (integer) array) 4-byte-utf-8))
(declaim (ftype (function (integer) t) encode-utf-8))
(declaim (ftype (function (stream) integer) decode-utf-8-from-stream))


; Floats

(declaim (ftype (function (float integer) array) dec-to-bit))
(declaim (ftype (function (float array array) array) make-32-bit-exponent))
(declaim (ftype (function (float array array) array) make-64-bit-exponent))
(declaim (ftype (function (float) (or array list)) float-to-32-bit))
(declaim (ftype (function (float) (or array list)) float-to-64-bit))
(declaim (ftype (function (array) integer) calculate-mantissa))
(declaim (ftype (function (array) float) 32-bit-to-float))
(declaim (ftype (function (array) float) 64-bit-to-float))

; Decimals

(declaim (ftype (function (number) list) split-decimal-string))
(declaim (ftype (function (number) array) decimal-to-32-bit))
(declaim (ftype (function (number) array) decimal-to-64-bit))
(declaim (ftype (function (array) number) 32-bit-to-decimal))
(declaim (ftype (function (array) number) 64-bit-to-decimal))

; Password

(declaim (ftype (function (string) array) make-8bit-char-array))
(declaim (ftype (function () t) make-salt))
(declaim (ftype (function (string t) t)  hash-argon2-password))
(declaim (ftype (function (string t) t)  hash-bcrypt-password))
(declaim (ftype (function (string t) t)  hash-scrypt-password))

; JLI

(declaim (ftype (function (array keyword symbol string &optional list) array) find-in-jli))
(declaim (ftype (function (array keyword &optional number) t) jli-entry))
(declaim (ftype (function (string stream)) write-string-to-stream))
(declaim (ftype (function (t stream)) write-element))
(declaim (ftype (function (hash-table stream)) write-hash-table))
(declaim (ftype (function (array stream)) write-array))
(declaim (ftype (function (array string)) write-jli))	

(declaim (ftype (function (stream) t) read-element))
(declaim (ftype (function (stream integer) hash-table) read-hash-table))
(declaim (ftype (function (stream integer) array) read-array))
(declaim (ftype (function (string) array) read-jli))

; JSON

(declaim (ftype (function (stream) string) read-json-key))
(declaim (ftype (function (stream)) read-json-value))
(declaim (ftype (function (stream) hash-table) json-record-to-hash))
(declaim (ftype (function (string) array) make-jli-from-json))
(declaim (ftype (function (stream) array) make-jli-from-json-stream))
(declaim (ftype (function (string) array) jli-from-json-file))
(declaim (ftype (function (hash-table) string) json-string-from-jli-record))
(declaim (ftype (function (array) string) json-string-from-jli))

; XML

(declaim (ftype (function (stream) keyword) read-tag-name))
(declaim (ftype (function (stream) string) read-xml-content))
(declaim (ftype (function (stream) string) get-xml-prolog))
(declaim (ftype (function (stream)) skip-xml-comment))
(declaim (ftype (function (stream) hash-table) read-xml-record))
(declaim (ftype (function (stream) array) xml-to-jli))
(declaim (ftype (function (string) array) import-xml-from-file))

; CSV

(declaim (ftype (function (stream) list)  csv-record-to-list))
(declaim (ftype (function (stream) array)  csv-record-to-list-array))
(declaim (ftype (function (string) array)  import-csv-from-file))
(declaim (ftype (function (array) array)  csv-array-to-jli))

; Datetime

(declaim (ftype (function (integer) array) datetime-from-timestamp))
(declaim (ftype (function (string) array)  datetime-from-string))
(declaim (ftype (function (string) array)  date-from-string))
(declaim (ftype (function () array)  now))
(declaim (ftype (function (integer integer integer) string)  date-to-string))
(declaim (ftype (function (integer integer integer) string)  date-to-iso-string))
(declaim (ftype (function (integer integer integer) string)  us-date-to-string))
(declaim (ftype (function (integer integer integer integer integer integer integer) string)  datetime-to-string))
(declaim (ftype (function (integer integer integer integer integer integer integer) string)  datetime-to-iso-string))
(declaim (ftype (function (integer integer integer integer integer integer integer) string)  datetime-to-iso-string-no-T))
(declaim (ftype (function (integer integer integer integer integer integer integer) string)  us-datetime-to-string))
(declaim (ftype (function (integer) string)  date-from-epoch))
(declaim (ftype (function (integer) string)  iso-date-from-epoch))
(declaim (ftype (function (integer) string)  no-t-iso-from-epoch))