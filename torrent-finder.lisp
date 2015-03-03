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

(defun make-handshake (info-hash &optional
                                   (peer-id +my-id+)
                                   (reserved +handshake-reserved-bytes+)
                                   (protocol-name +protocol-name+))
  (with-output-to-sequence (handshake)
    (write-byte (length protocol-name) handshake)
    (write-sequence (string-to-octets protocol-name) handshake)
    (write-sequence reserved handshake)
    (write-sequence info-hash handshake)
    (write-sequence peer-id handshake)))

(defclass peer ()
  ((peer-id :initarg :peer-id
            :reader peer-id)
   (reserved-bytes :initarg :reserved-bytes
                   :reader reserved-bytes)))

(defun peer-supports-metadata-sending? (peer)
  (supports-extension-protocol? (reserved-bytes peer)))

(defun read-handshake (stream)
  (let* ((protocol-name-length (read-byte stream))
         (protocol-name (make-binary-buffer protocol-name-length))
         (reserved-bytes (make-binary-buffer 8))
         (info-hash (make-binary-buffer 20))
         (peer-id (make-binary-buffer 20)))
    (read-sequence protocol-name stream)
    (read-sequence reserved-bytes stream)
    (read-sequence info-hash stream)
    (read-sequence peer-id stream)
    (values protocol-name reserved-bytes info-hash peer-id)))

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
    (values message-id payload)))

(defun connect-to-peer (address info-hash)
  (handler-case
      (with-connected-socket
          (peer-socket (socket-connect (host address)
                                       (port address)
                                       :timeout 5
                                       :element-type '(unsigned-byte 8)))
        (let ((peer-stream (socket-stream peer-socket)))
          (write-sequence (make-handshake info-hash) peer-stream)
          (force-output peer-stream)
          (wait-for-input peer-socket :timeout 5)
          (multiple-value-bind (protocol-name reserved-bytes info-hash peer-id)
              (read-handshake peer-stream)
            (log:debug "Protocol Name: ~a" (flex:octets-to-string protocol-name))
            (log:debug "Reserved bytes: ~a" (ironclad:byte-array-to-hex-string reserved-bytes))
            (log:debug "Info hash: ~a" (ironclad:byte-array-to-hex-string info-hash))
            (log:debug "Peer id: ~a" (ironclad:byte-array-to-hex-string peer-id)))
          (while t
            (wait-for-input peer-socket :timeout 5)
            (multiple-value-bind (message-id payload)
                (read-message peer-stream)
              (log:debug "Message id: ~d" message-id)
              (log-hexdump "Payload:" payload)))))
    (usocket:timeout-error ()
      (log:info "Peer ~a has no more data, disconnecting" address))
    (usocket:host-unreachable-error ()
      (log:warn "Peer ~a is not reachable" address))
    (usocket:connection-refused-error ()
      (log:warn "Peer ~a refused connection" address))))

(defun do-handshake (host port info-hash)
  (let ((handshake (make-handshake info-hash)))
    (with-connected-socket
        (sock (socket-connect host port :timeout 2 :element-type '(unsigned-byte 8)))
      (write-sequence handshake (socket-stream sock))
      (force-output (socket-stream sock))
      (wait-for-input sock :timeout 5)
      (tcp/read-data sock))))
