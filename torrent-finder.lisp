(in-package #:torrent-finder)

(defun decode-torrent-file (path)
  (with-open-file (torrent-file path :direction :input :element-type '(unsigned-byte 8))
    (bencoding/decode torrent-file)))

(defun torrent-file->info-hash (path)
  (with-open-file (torrent-file path :direction :input :element-type '(unsigned-byte 8))
    (let* ((decoded-file (bencoding/decode torrent-file))
           (info-encoded (bencoding/encode (gethash "info" decoded-file))))
      (ironclad:digest-sequence :sha1 (subseq info-encoded 0)))))

(defparameter +handshake-reserved-bytes+
  (make-array 8
              :element-type '(unsigned-byte 8)
              :initial-contents #( 0  0  0  0
                                   0  0  0  0)))

(defparameter +protocol-name+ "BitTorrent protocol")
(defparameter +my-id+ (ironclad:hex-string-to-byte-array
                       "2d4c54313030302d5a78586e5054425253797029"))

(defun supports-extension-protocol? (reserved-bytes)
  (logand (aref reserved-bytes 5) #x10))

(defclass handshake ()
  ((protocol-name :initarg :protocol-name :reader protocol-name)
   (reserved-bytes :initarg :reserved-bytes :reader reserved-bytes)
   (info-hash :initarg :info-hash :reader info-hash)
   (peer-id :initarg :peer-id :reader peer-id)))

(defmethod print-object ((handshake handshake) stream)
  (format stream "Protocol Name: ~a~%" (protocol-name handshake))
  (format stream "Reserved bytes: ~a~%"
          (ironclad:byte-array-to-hex-string (reserved-bytes handshake)))
  (format stream "Info hash: ~a~%"
          (ironclad:byte-array-to-hex-string (info-hash handshake)))
  (format stream "Peer id: ~a"
          (ironclad:byte-array-to-hex-string (peer-id handshake))))

(defun write-handshake (stream info-hash
                        &optional
                          (peer-id +my-id+)
                          (reserved +handshake-reserved-bytes+)
                          (protocol-name +protocol-name+))
  (write-byte (length protocol-name) stream)
  (write-sequence (string-to-octets protocol-name :external-format :ascii) stream)
  (write-sequence reserved stream)
  (write-sequence info-hash stream)
  (write-sequence peer-id stream))

(defun read-handshake (stream)
  (let* ((protocol-name (read-length-prefixed-string stream))
         (reserved-bytes (make-binary-buffer 8))
         (info-hash (make-binary-buffer 20))
         (peer-id (make-binary-buffer 20)))
    (read-sequence reserved-bytes stream)
    (read-sequence info-hash stream)
    (read-sequence peer-id stream)
    (make-instance 'handshake
                   :protocol-name protocol-name
                   :reserved-bytes reserved-bytes
                   :info-hash info-hash
                   :peer-id peer-id)))

(defclass peer ()
  ((peer-socket :initarg :peer-socket :accessor peer-socket :initform nil)
   (peer-id :initarg :peer-id :accessor peer-id :initform nil)
   (reserved-bytes :initarg :reserved-bytes :accessor reserved-bytes :initform nil)
   (info-hash :initarg :info-hash :reader info-hash)
   (extension-messages :accessor extension-messages :initform nil)))

(defmethod print-object ((peer peer) stream)
  (format stream "<peer-socket: ~a, peer-id: ~a>"
          (peer-socket peer)
          (if (peer-id peer)
              (ironclad:byte-array-to-hex-string (peer-id peer))
              nil)))

(defun read-length-prefixed-string (stream)
  "[1 byte = str length][<str length bytes> = str]"
  (let* ((result-length (read-byte stream))
         (result-buffer (make-binary-buffer result-length)))
    (read-sequence result-buffer stream)
    (octets-to-string result-buffer :external-format :utf-8)))

(defclass peer-message ()
  ((msg-id :initarg :msg-id :reader msg-id)
   (msg-payload :initarg :msg-payload :accessor msg-payload)))

(defmethod print-object ((msg peer-message) stream)
  (format stream "<id: ~d, payload: ~a>"
          (msg-id msg)
          (ironclad:byte-array-to-hex-string (msg-payload msg))))

(defun read-message (stream)
  (let* ((message-length-bytes (make-binary-buffer 4))
         (message-length 0)
         (message-id nil)
         (payload nil))
    (read-sequence message-length-bytes stream)
    ; remove message-id byte
    (setf message-length (- (octets-to-integer message-length-bytes) 1))
    (setf message-id (read-byte stream))
    (when (> message-length 0)
      (setf payload (make-binary-buffer message-length))
      (read-sequence payload stream))
    (make-instance 'peer-message :msg-id message-id :msg-payload payload)))

;; (defun connect-to-peer (address info-hash)
;;   (handler-case
;;       (with-connected-socket
;;           (peer-socket (socket-connect (host address)
;;                                        (port address)
;;                                        :timeout 4
;;                                        :element-type '(unsigned-byte 8)))
;;         (let ((peer-stream (socket-stream peer-socket)))
;;           (log:debug "Sending handshake to peer ~a" address)
;;           (write-handshake peer-stream info-hash)
;;           (force-output peer-stream)
;;           (wait-for-input peer-socket :timeout 5)
;;           (let ((handshake-response (read-handshake peer-stream)))
;;             (log:debug "Handshake response:~%~a" handshake-response))
;;           (while t
;;             (wait-for-input peer-socket :timeout 5)
;;             (multiple-value-bind (message-id payload)
;;                 (read-message peer-stream)
;;               (log:debug "Message id: ~d" message-id)
;;               (log:debug "Payload:~%~a" (hexdump payload))))))
;;     (usocket:timeout-error ()
;;       (log:info "Peer ~a has no more data, disconnecting" address))
;;     (usocket:host-unreachable-error ()
;;       (log:warn "Peer ~a is not reachable" address))
;;     (usocket:connection-refused-error ()
;;       (log:warn "Peer ~a refused connection" address))))

(defun connect-to-peer (address info-hash)
  (handler-case
      (make-instance
       'peer
       :peer-socket (socket-connect (host address) (port address)
                                    :timeout 4
                                    :element-type '(unsigned-byte 8))
       :info-hash info-hash)
    (usocket:timeout-error ()
      (log:info "Timeout on peer connection ~a" address))
    (usocket:host-unreachable-error ()
      (log:warn "Peer ~a is not reachable" address))
    (usocket:connection-refused-error ()
      (log:warn "Peer ~a refused connection" address))))

(defmacro with-connected-peer ((var peer) &body body)
  `(let ((,var ,peer))
     (unwind-protect
          (handler-case
              (when ,var
                ,@body)
            (usocket:timeout-error ()
              (log:warn "Timeout on peer connection ~a" ,var))
            (end-of-file ()
              (log:warn "Prematue end of file on peer socket ~a" ,var)))
       (when (and ,var (peer-socket ,var))
         (log:debug "Closing peer socket ~a" (peer-socket ,var))
         (socket-close (peer-socket ,var))))))

(defun perform-handshake (peer)
  (let ((peer-stream (socket-stream (peer-socket peer))))
    (write-handshake peer-stream (info-hash peer))
    (force-output peer-stream)
    (if (wait-for-input (peer-socket peer) :timeout 5 :ready-only t)
        (if (socket-has-data? (peer-socket peer))
            (let ((handshake-response (read-handshake peer-stream)))
              (log:debug "Handshake response:~%~a" handshake-response)
              (setf (peer-id peer) (peer-id handshake-response))
              (setf (reserved-bytes peer) (reserved-bytes handshake-response))
              t)
            (error 'end-of-file))
        (error 'usocket:timeout-error))))

(defun make-metadata-handshake ()
  (let ((result (make-hash-table :test 'equalp))
        (mdict (make-hash-table :test 'equalp)))
    (setf (gethash "ut_metadata" mdict) 8)
    (setf (gethash "m" result) mdict)
    (setf (gethash "metadata_size" result) 8864)
    (with-output-to-sequence (mhnd)
      (let ((fff (with-output-to-sequence (out)
                   (write-byte 20 out)
                   (write-byte 0 out)
                   (write-sequence (bencoding/encode result) out))))
        (write-sequence (ironclad:integer-to-octets (length fff) :n-bits 32) mhnd)
        (write-sequence fff mhnd)))))

(defun perform-extended-handshake (peer)
  (let ((extended-handshake (make-metadata-handshake)))
    (write-sequence extended-handshake (socket-stream (peer-socket peer)))
    (force-output (socket-stream (peer-socket peer)))))

(defun add-prefix-message-length (msg-buffer)
  (with-output-to-sequence (msg)
    (write-sequence (ironclad:integer-to-octets (length msg-buffer) :n-bits 32) msg)
    (write-sequence msg-buffer msg)))

(defun make-metadata-request (msg-id piece)
  (let ((extended-msg (make-hash-table :test 'equal)))
    (setf (gethash "msg_type" extended-msg) 0)
    (setf (gethash "piece" extended-msg) piece)
    (let ((msg-data (with-output-to-sequence (tmp)
                      (write-byte 20 tmp)
                      (write-byte msg-id tmp)
                      (write-sequence (bencoding/encode extended-msg) tmp))))
      (add-prefix-message-length msg-data))))

(defun download-metadata-from (address info-hash)
  (with-connected-peer (peer (connect-to-peer address info-hash))
    (perform-handshake peer)
    (when (supports-extension-protocol? (reserved-bytes peer))
      (perform-extended-handshake peer)
      (while (and (wait-for-input (peer-socket peer) :timeout 10 :ready-only t)
                  (socket-has-data? (peer-socket peer)))
        (let ((msg (read-message (socket-stream (peer-socket peer)))))
          (log:trace msg)
          (when (= (msg-id msg) 20)
            (let* ((payload-stream (flex:make-in-memory-input-stream (msg-payload msg)))
                   (extended-msg-id (read-byte payload-stream)))
              (if (= extended-msg-id 0)
                  (progn
                    (setf (extension-messages peer)
                          (gethash "m" (bencoding/decode payload-stream)))
                    (loop for value being the hash-values of
                         (extension-messages peer) using (hash-key key)
                       do (log:debug "~a -> ~d" key value))
                    (let ((metadata-request
                           (make-metadata-request (gethash "ut_metadata"
                                                           (extension-messages peer))
                                                  0)))
                      (log:debug "Sending metadata request:~%~a" (hexdump metadata-request))
                      (write-sequence metadata-request (socket-stream (peer-socket peer)))
                      (force-output (socket-stream (peer-socket peer)))))
                  (if (= (gethash "ut_metadata" (extension-messages peer)) extended-msg-id)
                      (progn
                        (log:debug "got ut_metadata message:~%~a" (msg-payload msg)))
                      (progn
                        (log:debug "got extended message of id ~d" extended-msg-id)))))))))))
