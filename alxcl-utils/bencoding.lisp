(in-package #:alxcl-utils)

(defparameter +ascii+ (make-external-format :ascii))

(define-condition invalid-dict-key (error)
  ((key :initarg :key
        :reader key)))

(defun dict->sorted-list (dict)
  (let ((result))
    (maphash (lambda (key val)
               (if (or (stringp key) (arrayp key))
                   (push (cons key val) result)
                   (error 'invalid-dict-key :key key)))
             dict)
    (sort result #'string< :key #'car)))

;; Encoding
;; ================================================================================

(defgeneric bencoding/encode (item)
  (:documentation "Encodes: integer, string, list, dictionary, binary array"))

(defgeneric bencoding/encode-to-stream (item stream)
  (:documentation "Encodes: integer, string, list, dictionary, binary array"))

(defmacro def-encode (item-class)
  `(defmethod bencoding/encode ((item ,item-class))
     (bencoding/encode-to-stream item nil)))

(defmacro def-encode-without-stream (item-class)
  `(defmethod bencoding/encode-to-stream ((item ,item-class) (stream (eql nil)))
     (with-output-to-sequence (out) (bencoding/encode-to-stream item out))))

(defmacro def-encode-to-stream ((item-var item-class stream-var) &body body)
  `(progn
     (def-encode-without-stream ,item-class)
     (defmethod bencoding/encode-to-stream ((,item-var ,item-class) ,stream-var) ,@body)))

(def-encode integer)
(def-encode string)
(def-encode array)
(def-encode list)
(def-encode hash-table)

(def-encode-to-stream (int integer stream)
  (write-sequence (string-to-octets (format nil "i~ae" int) :external-format +ascii+) stream))

(def-encode-to-stream (str string stream)
  (write-sequence (string-to-octets (format nil "~a:" (length str)) :external-format +ascii+) stream)
  (write-sequence (string-to-octets str :external-format :utf-8) stream))

(def-encode-to-stream (arr array stream)
  (write-sequence (string-to-octets (format nil "~a:" (length arr)) :external-format +ascii+) stream)
  (write-sequence arr stream))

(def-encode-to-stream (list list stream)
  (write-byte (char-code #\l) stream)
  (loop for item in list do (bencoding/encode-to-stream item stream))
  (write-byte (char-code #\e) stream))

(def-encode-to-stream (dict hash-table stream)
  (write-byte (char-code #\d) stream)
  (loop for keyval in (dict->sorted-list dict) do
       (destructuring-bind (key . val) keyval
         (bencoding/encode-to-stream key stream)
         (bencoding/encode-to-stream val stream)))
  (write-byte (char-code #\e) stream))

;; Decoding
;; ================================================================================

(define-condition invalid-bencoded-type (error)
  ((octet :initarg :octet
          :reader octet)))

(define-condition invalid-bencoding (error)
  ((message :initarg :message
            :reader message)))

(defgeneric bencoding/decode (input))

(defmethod bencoding/decode ((str string))
  (bencoding/decode (make-in-memory-input-stream (string-to-octets str))))

(defmethod bencoding/decode ((seq sequence))
  (with-input-from-sequence (stream seq)
    (bencoding/decode (make-flexi-stream stream :external-format :utf-8))))

(defmethod bencoding/decode ((stream stream))
  (bencoding/decode (make-flexi-stream stream)))

(defmethod bencoding/decode ((stream flexi-stream))
  (let ((first-char (code-char (peek-byte stream))))
    (case first-char
      (#\i (decode-integer stream))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (decode-array stream))
      (#\l (decode-list stream))
      (#\d (decode-dict stream))
      (t (error 'invalid-bencoded-type :octet first-char)))))

(defun validate-integer (digits negative? integer)
  (assert (or (and (= integer 0)
                   (and (not negative?)
                        (= (length digits) 1)))
              (and (> integer 0)
                   (not (char= (elt digits 0) #\0))))
          (digits)
          "Invalid bencoded integer: ~a" digits))

(defun decode-integer (stream)
  (read-expected-byte stream (char-code #\i))
  (let* ((negative? (read-possible-byte stream (char-code #\-)))
         (digits (read-ascii-digits stream))
         (integer (parse-integer digits)))
    (validate-integer digits negative? integer)
    (read-expected-byte stream (char-code #\e))
    (if negative?
        (- integer)
        integer)))

(defun decode-array (stream)
  (let ((length (parse-integer (read-ascii-digits stream))))
    (read-expected-byte stream (char-code #\:))
    (read-octets stream length)))

(defun decode-string (stream)
  (octets-to-string (decode-array stream) :external-format :utf-8 ))

(defun decode-list (stream)
  (read-expected-byte stream (char-code #\l))
  (loop until (read-byte-if-equals stream (char-code #\e)) collect (bencoding/decode stream)))

(defun decode-dict (stream)
  (read-expected-byte stream (char-code #\d))
  (let ((result (make-hash-table :test 'equal)))
    (loop until (read-byte-if-equals stream (char-code #\e)) do
         (setf (gethash (decode-string stream) result)
               (bencoding/decode stream)))
    result))
