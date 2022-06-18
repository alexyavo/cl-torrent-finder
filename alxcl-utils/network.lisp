(in-package #:alxcl-utils)

(defclass endpoint-address ()
  ((host :initarg :host
         :reader host)
   (port :initarg :port
         :reader port)))

(defmethod print-object ((address endpoint-address) stream)
  (let ((host (host address)))
    (if (stringp host)
        (format stream "~a:~d" host (port address))
      (format stream "~a:~d" (ipv4-vec->string host) (port address)))))

(defun make-address (host port)
  (make-instance 'endpoint-address :host host :port port))

(defun ipv4-vec->string (ipv4-vec)
  "Converts 4 byte vector to ipv4 string address"
  (with-output-to-string (result)
    (write-string (write-to-string (aref ipv4-vec 0)) result)
    (write-string "." result)
    (write-string (write-to-string (aref ipv4-vec 1)) result)
    (write-string "." result)
    (write-string (write-to-string (aref ipv4-vec 2)) result)
    (write-string "." result)
    (write-string (write-to-string (aref ipv4-vec 3)) result)))

(defun ipv4-vec->endpoint-address (ipv4-vec)
  (make-instance 'endpoint-address
                 :host (subseq ipv4-vec 0 4)
                 :port (ironclad:octets-to-integer (subseq ipv4-vec 4 6))))

(defmethod print-object ((socket usocket:stream-usocket) stream)
  (format stream "<TCP socket: (local) ~a:~d <-> (remote) ~a:~d>"
          (ipv4-vec->string (usocket:get-local-address socket))
          (usocket:get-local-port socket)
          (ipv4-vec->string (usocket:get-peer-address socket))
          (usocket:get-peer-port socket)))

(defmethod print-object ((socket usocket:stream-server-usocket) stream)
  (format stream "<TCP server socket: ~a:~d"
          (ipv4-vec->string (usocket:get-local-address socket))
          (usocket:get-local-port socket)))

(defun socket-has-data? (socket)
  (listen (usocket:socket-stream socket)))

(defvar *initial-data-buffer-size* 128)
(defun tcp/read-data (socket)
  (log:trace "Reading from ~a" socket)
  (let ((data (make-array *initial-data-buffer-size*
                          :fill-pointer 0
                          :adjustable t
                          :element-type '(unsigned-byte 8))))
    (loop :while (socket-has-data? socket) :do
       (vector-push-extend (read-byte (usocket:socket-stream socket)) data))
    (log:debug "Read ~d bytes from ~a:~%~a" (length data) socket (hexdump data))
    data))

(defmacro with-sockets ((sockets-var) &body body)
  "On exit closes all sockets that were added to list"
  `(let ((,sockets-var '()))
     (unwind-protect
          (progn
            ,@body)
       (loop :for socket :in ,sockets-var :do
          (log:trace "Closing ~a" socket)
          (usocket:socket-close socket)))))

(defun on-connect (socket)
  (log:debug "Got connection,  ~a" socket)
  t)

(defun on-data (socket)
  (log:trace socket)
  (tcp/read-data socket))

(defun tcp/server (host port &key async on-connect on-data)
  (declare (ignore async))

  (with-sockets (sockets)
    (labels ((add-socket (socket)
               (setf sockets (push socket sockets)))

             (remove-socket (socket)
               (log:trace "removing ~a" socket)
               (setf sockets (delete socket sockets))
               (usocket:socket-close socket))

             (accept-new-connection (socket)
               (let ((conn (usocket:socket-accept socket)))
                 (if (funcall on-connect conn)
                     (progn
                       (log:info "on-connect callback accepted connection from ~a" socket)
                       (add-socket conn))
                     (progn
                       (log:info "on-connect callback rejected connection from ~a" socket)
                       (usocket:socket-close socket)
                       nil))))

             (handle-client-socket (socket)
               (if (socket-has-data? socket)
                   (funcall on-data socket)
                   (remove-socket socket))))

      (log:info "Starting TCP server on ~a:~d" host port)
      (let ((listen-socket
             (usocket:socket-listen host port
                                    :reuse-address t
                                    :element-type '(unsigned-byte 8))))
        (add-socket listen-socket)
        (while t
          (loop :for socket :in (usocket:wait-for-input sockets :ready-only t) :do
             (if (eq socket listen-socket)
                 (accept-new-connection socket)
                 (handle-client-socket socket))))))))

(defun udp/default-handler (buffer)
  (log:debug "UDP server received:~%~a" (hexdump buffer))
  (sleep 5)
  buffer)

;;run the server
;; (setf tmp (usocket:socket-server "127.0.0.1" 8283 #'udp/default-handler nil
;;                                  :protocol :datagram :in-new-thread t))

(defun udp/send-receive (address buffer)
  (log:trace "Sending to ~a:~%~a" address (hexdump buffer))
  (usocket:with-connected-socket
      (socket (usocket:socket-connect (host address) (port address)
                                      :protocol :datagram
                                      :timeout 2))
    (usocket:socket-send socket buffer (length buffer))
    (if (usocket:wait-for-input socket :timeout 2 :ready-only t)
        (multiple-value-bind (response length) (usocket:socket-receive socket nil 8192)
          (let ((response-buffer (subseq response 0 length)))
            (log:trace "Received from ~a (~d bytes):~%~a"
                       address length (hexdump response-buffer))
            response-buffer))
        (error 'usocket:timeout-error))))
