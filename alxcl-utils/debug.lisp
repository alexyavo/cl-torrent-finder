(in-package :alxcl-utils)

(defun hexdump-to-stream (stream seq &key (address-length 8) (address-offset 0))
  (labels ((x->char (x)
             (let ((c (code-char x)))
               (if (and (standard-char-p c) (graphic-char-p c)) c #\.)))

           (x->str (l)
             (coerce (mapcar #'x->char l) 'string))

           (append-header ()
             (format stream "~a" (make-string address-length :initial-element #\=))
             (let ((l '#.(loop for n below 16 collect n)))
               (format stream "== ~{+~x ~}==================~%" l)))

           (append-address (i)
             (format stream (format nil "~~~a,'0x : " address-length)
                     (+ address-offset i)))

           (append-byte (b)
             (format stream "~2,'0x " b))

           (append-text (l)
             (format stream "| ~a~%" (x->str l)))

           (append-padding (n)
             (format stream "~a" (make-string (* n 3) :initial-element #\space)))

           (reduce-fn (state byte)
             (destructuring-bind (col row txt) state
               (when (and (zerop col) (zerop (mod row 10)))
                 (append-header))
               (when (zerop col)
                 (append-address (* row 16)))
               (append-byte byte)
               (when (= col 15)
                 (append-text (nreverse (cons byte txt))))
               (if (= col 15)
                   (list 0 (1+ row) nil)
                   (list (1+ col) row (cons byte txt))))))

    (destructuring-bind (col row txt)
        (reduce #'reduce-fn seq :initial-value (list 0 0 nil))
      (declare (ignore row))
      (unless (zerop col)
        (append-padding (- 16 col))
        (append-text (nreverse txt))))))

(defun hexdump (seq &key (address-length 8) (address-offset 0))
  (with-output-to-string (result)
    (hexdump-to-stream result seq
                       :address-length address-length
                       :address-offset address-offset)))
