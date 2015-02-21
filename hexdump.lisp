(in-package :torrent-finder)

(defun hex-dump (seq &key (address-length 8) (address-offset 0))
  (labels ((x->char (x)
                    (let ((c (code-char x)))
                      (if (and (standard-char-p c) (graphic-char-p c)) c #\.)))
           (x->str (l)
                   (coerce (mapcar #'x->char l) 'string))
           (print-header ()
                         (princ (make-string address-length :initial-element #\=))
                         (let ((l '#.(loop for n below 16 collect n)))
                           (format t "== ~{+~x ~}=================~%" l)))
           (print-address (i)
                          (format t (format nil "~~~a,'0x : " address-length)
                                  (+ address-offset i)))
           (print-byte (b)
                       (format t "~2,'0x " b))
           (print-text (l)
                       (format t "| ~a~%" (x->str l)))
           (print-padding (n)
                          (princ (make-string (* n 3) :initial-element #\space)))
           (reduce-fn (state byte)
                      (destructuring-bind (col row txt) state
                        (when (and (zerop col) (zerop (mod row 10)))
                          (print-header))
                        (when (zerop col)
                          (print-address (* row 16)))
                        (print-byte byte)
                        (when (= col 15)
                          (print-text (nreverse txt)))
                        (if (= col 15)
                            (list 0 (1+ row) nil)
                          (list (1+ col) row (cons byte txt))))))
    (fresh-line)
    (destructuring-bind (col row txt)
        (reduce #'reduce-fn seq :initial-value (list 0 0 nil))
      (declare (ignore row))
      (unless (zerop col)
        (print-padding (- 16 col))
        (print-text (nreverse txt))))
    (fresh-line)))

(defun hex-dump-word (address)
  #+sbcl
  (format nil "~8,'0X"
          (sb-alien:deref
           (sb-alien:sap-alien
            (sb-alien::int-sap address)
            (* (sb-alien:unsigned 32)))))
  #-sbcl (format nil "not yet implemented!"))

(defun hex-dump-byte (address)
  #+sbcl
  (format nil "~2,'0X"
          (sb-alien:deref
           (sb-alien:sap-alien
            (sb-alien::int-sap address)
            (* (sb-alien:unsigned 8)))))
  #-sbcl (format nil "not yet implemented!"))

(defun hex-dump-memory (start-address length)
  (loop for i from start-address below (+ start-address length)
        collect (format nil (hex-dump-byte i))))

(defun hex-dump-words (start-address length)
  (loop for i from start-address below (+ start-address length) by 4
        collect (format nil (hex-dump-word i))))

(defun hex-dump-long (address)
  (hex-dump-memory address 4))

(defun char-dump-byte (address)
  #+sbcl
  (format nil "~A"
          (code-char
           (sb-alien:deref
            (sb-alien:sap-alien
             (sb-alien::int-sap address)
             (* (sb-alien:unsigned 8))))))
  #-sbcl (format nil "not yet implemented"))

(defun char-dump-memory (start-address length)
  (loop for i from start-address below (+ start-address length)
        collect (format nil (char-dump-byte i))))

(defun double-at-address (address)
  #+sbcl
  (sb-alien:deref
   (sb-alien:sap-alien
    (sb-alien::int-sap address)
    (* (sb-alien:double-float))))
  #-sbcl (format nil "not yet implemented!"))

(defun double-dump-memory (start-address length)
  #+sbcl
  (let ((size (sb-alien:alien-size sb-alien:double-float :bytes)))
    (loop for i from start-address
          below (+ start-address (*  length size))
          by size
          collect (cons (format nil "~X" i)
                        (double-at-address i))))
  #-sbcl (format nil "not yet implemented!"))
