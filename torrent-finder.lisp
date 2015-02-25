;;;; torrent-finder.lisp

(in-package #:torrent-finder)

;;; "torrent-finder" goes here. Hacks and glory await!

(defun print-hash-table-entry (key value)
  (format *standard-output* "~S: ~S~%" key value))

(defun print-hash-table (hash-table)
  (with-hash-table-iterator
      (it hash-table)
    (loop (multiple-value-bind (entry? key value) (it)
            (if entry?
                (print-hash-table-entry key value)
                (return))))))

(defun host-vec->string (host-vec)
  (with-output-to-string
      (result)
    (write-string (write-to-string (aref host-vec 0)) result)
    (write-string "." result)
    (write-string (write-to-string (aref host-vec 1)) result)
    (write-string "." result)
    (write-string (write-to-string (aref host-vec 2)) result)
    (write-string "." result)
    (write-string (write-to-string (aref host-vec 3)) result)))

(defclass endpoint-address ()
  ((host :initarg :host
         :initform (error "must provide host")
         :accessor host)
   (port :initarg :port
         :initform (error "must provide port")
         :accessor port)))

(defmethod print-object ((address endpoint-address) stream)
  (let ((host (host address)))
    (if (vectorp host)
        (format stream "~a:~d" (host-vec->string host) (port address))
        (format stream "~a:~d" (host address) (port address)))))

(defparameter +node-id-length+ 20)

(defparameter +default-transaction-id+ "aa")
(defparameter +default-node-id+ "abcdefghij0123456789")

(defclass dht/query-base ()
  ((query-id :initarg :query-id
             :initform +default-transaction-id+
             :accessor query-id)
   (issuer-id :initarg :issuer-id
              :initform +default-node-id+
              :accessor issuer-id)))

(defparameter +compact-node-adress-length+ 6)

(defclass dht/compact-node-info ()
  ((node-id :initarg :node-id
            :initform (error "node-id cannot be null")
            :accessor node-id)
   (node-address :initarg :node-address
                 :initform (error "node address cannot be null")
                 :accessor node-address)))

(defmethod print-object ((node-info dht/compact-node-info) stream)
  (format stream "[~a, ~a]"
          (byte-array-to-hex-string (node-id node-info))
          (node-address node-info)))

(defun compact-address->endpoint-address (compact-address)
  (let ((host (subseq compact-address 0 4))
        (port-vec (subseq compact-address 4 6))
        (port 0))
    (setf (ldb (byte 8 0) port) (aref port-vec 1))
    (setf (ldb (byte 8 8) port) (aref port-vec 0))
    (make-instance 'endpoint-address
                   :host host
                   :port port)))

(defun read-node-id (stream)
  (let ((node-id (make-array +node-id-length+ :element-type '(unsigned-byte 8))))
    (read-sequence node-id stream)
    node-id))

(defun read-node-address (stream)
  (let ((compact-address (make-array +compact-node-adress-length+
                                     :element-type '(unsigned-byte 8))))
    (read-sequence compact-address stream)
    (compact-address->endpoint-address compact-address)))

(defun read-compact-node-info (stream)
  (make-instance 'dht/compact-node-info
                 :node-id (read-node-id stream)
                 :node-address (read-node-address stream)))

(defparameter +default-query-base+ (make-instance 'dht/query-base))

(defparameter +max-response-bytes+ 8196)

(defparameter +transaction-id-key+ "t")
(defparameter +message-type-key+ "y")
(defparameter +query-method-key+ "q")
(defparameter +args-key+ "a")
(defparameter +response-args-key+ "r")

(defparameter +query-message+ "q")
(defparameter +error-message+ "e")

(defparameter +error-description-list-key+ "e")
(defparameter +id-key+ "id")

(defparameter +router-bittorrent+
  (make-instance 'endpoint-address :host "router.bittorrent.com" :port 6881))

(defparameter *response-buffer*
  (make-array +max-response-bytes+ :element-type '(unsigned-byte 8)))

(defun make-query-base (query-name query-id query-issuer-id)
  (let ((query (make-hash-table :test 'equal)))
    (setf (gethash +transaction-id-key+ query) query-id)
    (setf (gethash +message-type-key+ query) +query-message+)
    (setf (gethash +query-method-key+ query) query-name)
    (setf (gethash +args-key+ query) (make-hash-table :test 'equal))
    (add-query-arg query +id-key+ query-issuer-id)
    query))

(defun add-query-arg (query arg-key arg-value)
  (let ((args-dict (gethash +args-key+ query)))
    (setf (gethash arg-key args-dict) arg-value))
  query)

(defun make-ping-query (&optional (query-info +default-query-base+))
  (bencoding/encode (make-query-base "ping" (query-id query-info) (issuer-id query-info))))

(defun make-find-node-query (target-id &optional (query-info +default-query-base+))
  (bencoding/encode
   (add-query-arg
    (make-query-base "find_node" (query-id query-info) (issuer-id query-info))
    "target" target-id)))

(defvar *test-info-hash*
  (make-array +node-id-length+
              :element-type '(unsigned-byte 8)
              :initial-contents '(#x02 #x06 #x87 #x4E
                                  #xF6 #x5C #x89 #x09
                                  #xA7 #x97 #xF3 #x37
                                  #x66 #xE8 #x66 #x26
                                  #x7E #x57 #x4A #x10)))

(defun make-get-peers-query (info-hash &optional (query-info +default-query-base+))
  (bencoding/encode
   (add-query-arg
    (make-query-base "get_peers" (query-id query-info) (issuer-id query-info))
    "info_hash" info-hash)))

(defun make-announce-peer-query (info-hash port token
                                 &optional
                                   (implied-port nil)
                                   (query-info +default-query-base+))
  (let ((result (make-query-base "announce_peer"
                                 (query-id query-info)
                                 (issuer-id query-info))))
    (add-query-arg result "info_hash" info-hash)
    (add-query-arg result "port" port)
    (add-query-arg result "token" token)
    (when implied-port
      (add-query-arg result "implied_port" 1))
    (bencoding/encode result)))

(defun error-response? (msg-dict)
  (= (elt (gethash +message-type-key+ msg-dict) 0) (char-code #\e)))

(defun perform-query (query address)
  (with-connected-socket
      (query-socket (socket-connect (host address) (port address)
                                    :protocol :datagram
                                    :timeout 1))
    (socket-send query-socket query (length query))
    (multiple-value-bind (response-buffer response-length)
        (socket-receive query-socket nil +max-response-bytes+)
      (log:debug "Recieved response:~%~a"
                 (octets-to-string response-buffer :start 0 :end response-length))
      (log-hexdump "Response dump" (subseq response-buffer 0 response-length))
      (bencoding/decode (subseq response-buffer 0 response-length)))))

(defun parse-compact-nodes-info (info-vec)
  (let ((nodes-stream (make-flexi-stream (make-in-memory-input-stream info-vec))))
    (loop until (null (peek-byte nodes-stream nil nil nil))
       collect (read-compact-node-info nodes-stream))))

(defun parse-peer-values (values)
  (loop for value in values collect (compact-address->endpoint-address value)))

(defclass error-response ()
  ((error-code :initarg :error-code
               :reader error-code)
   (error-msg :initarg :error-msg
              :reader error-msg)))

(defmethod print-object ((err error-response) stream)
  (format stream "Error ~d: ~a" (error-code err) (error-msg err)))

(defun make-error-response (response-error)
  (make-instance 'error-response
                 :error-code (first response-error)
                 :error-msg (octets-to-string (second response-error))))

(defun dht/ping (address)
  (let ((response-dict (perform-query (make-ping-query) address)))
    (if (error-response? response-dict)
        (gethash +error-description-list-key+ response-dict)
        (let ((responder-id
               (gethash +id-key+ (gethash +response-args-key+ response-dict))))
          (values responder-id (byte-array-to-hex-string responder-id))))))

(defun dht/find-node (address target-node-id)
  (let ((response-dict (perform-query (make-find-node-query target-node-id) address)))
    (if (error-response? response-dict)
        (gethash +error-description-list-key+ response-dict)
        (parse-compact-nodes-info
         (gethash "nodes" (gethash +response-args-key+ response-dict))))))

(defclass get-peers-response ()
  ((peer-nodes :initarg :peer-nodes
               :initform nil
               :accessor peer-nodes)
   (peer-values :initarg :peer-values
                :initform nil
                :accessor peer-values)
   (token :initarg :token
          :initform nil
          :accessor token)))

(defmethod initialize-instance :after ((response get-peers-response) &key)
  (with-slots (peer-nodes peer-values token) response
    (when peer-nodes
      (setf peer-nodes (parse-compact-nodes-info peer-nodes)))
    (when peer-values
      (setf peer-values (parse-peer-values peer-values)))))

(defmethod print-object ((response get-peers-response) stream)
  (with-slots (peer-nodes peer-values token) response
    (format stream "peer-nodes: ~a~%peer-values: ~a~%token: ~a"
            peer-nodes peer-values token)))

(defun dht/get-peers (address info-hash)
  (let ((response-dict (perform-query (make-get-peers-query info-hash) address)))
    (if (error-response? response-dict)
        (gethash +error-description-list-key+ response-dict)
        (let* ((response-args (gethash +response-args-key+ response-dict)))
          (make-instance 'get-peers-response
                         :peer-nodes (gethash "nodes" response-args)
                         :peer-values (gethash "values" response-args)
                         :token (gethash "token" response-args))))))

(defun dht/announce-peer (address info-hash port token &key implied-port)
  (let ((response-dict
         (perform-query (make-announce-peer-query info-hash port token implied-port)
                        address)))
    (if (error-response? response-dict)
        (make-error-response (gethash +error-description-list-key+ response-dict))
        (let ((responder-id
               (gethash +id-key+ (gethash +response-args-key+ response-dict))))
          (values responder-id (byte-array-to-hex-string responder-id))))))
