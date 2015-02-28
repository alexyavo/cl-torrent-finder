(in-package #:torrent-dht)

;; (defmacro make-endpoint-addresses (&rest endpoints)
;;   (loop for (,first-var ,second-var) on ,list by #'cddr do ,@body)
;;   (with-gensyms (addresses host port)
;;     (let ((addresses (loop for (host port) on endpoints by #'cddr collect
;;                           (make-instance 'endpoint-address :host host :port port))))
;;       (loop for addr in addresses do))
;;     `(list
;;       (iterate-pair (,host ,port ,endpoints)
;;         (make-instance 'endpoint-address :host ,host :port ,port)))))

(defparameter +well-known-nodes-addresses+
  (list
   (make-instance 'endpoint-address :host "router.bittorrent.com" :port 6881)))

(defparameter +id-length+ 20)

(defparameter +max-response-bytes+ 8196)
(defparameter *response-buffer*
  (make-array +max-response-bytes+ :element-type '(unsigned-byte 8)))

;; Convenience definitions for debugging / interacting with DHT
(defvar *default-query-id* "aa")
(defvar *default-id* "abcdefghij0123456789")

(defclass node-info ()
  ((node-id :initarg :node-id
            :reader node-id)
   (node-address :initarg :node-address
                 :reader node-address)))

(defmethod print-object ((node node-info) stream)
  (format stream "[~a, ~a]"
          (byte-array-to-hex-string (node-id node))
          (node-address node)))

(defclass error-response ()
  ((error-code :initarg :error-code
               :reader error-code)
   (error-msg :initarg :error-msg
              :reader error-msg)))

(defmethod print-object ((err error-response) stream)
  (format stream "Error ~d: ~a" (error-code err) (error-msg err)))

(defun parse-peer-values (values)
  (loop for value in values collect (ipv4-vec->endpoint-address value)))

(defclass get-peers-response ()
  ((peer-nodes :initarg :peer-nodes
               :initform nil
               :reader peer-nodes)
   (peer-values :initarg :peer-values
                :initform nil
                :reader peer-values)
   (token :initarg :token
          :initform nil
          :reader token)))

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

(defun read-compact-address (stream)
  (make-instance 'endpoint-address
                 :host (read-octets stream 4)
                 :port (read-uint32 stream)))

(defun read-compact-node-info (stream)
  (make-instance 'node-info
                 :node-id (read-octets stream +id-length+)
                 :node-address (read-compact-address stream)))

(defun add-query-args (query &rest args)
  (let ((query-args (gethash "a" query (make-hash-table :test 'equal))))
    (iterate-pair (arg-key arg-val args)
      (setf (gethash arg-key query-args) arg-val))
    (setf (gethash "a" query) query-args))
  query)

(defvar *query-id* *default-query-id*)
(defvar *node-id* *default-id*)

(defun make-query (query-name)
  "Creates a DHT query. Variables *query-id* and *node-id* should be
rebound on need, otherwise they are bound to default values."
  (let ((query (make-hash-table :test 'equal)))
    (setf (gethash "y" query) "q"
          (gethash "q" query) query-name
          (gethash "t" query) *query-id*)
    (add-query-args query "id" *node-id*)))

(defmacro with-encoded-query ((query name) &body body)
  "Encodes query on exit"
  `(let ((,query (make-query ,name)))
     ,@body
     (bencoding/encode ,query)))

(defun make-ping-query ()
  (with-encoded-query (result "ping")))

(defun make-find-node-query (target)
  (with-encoded-query (result "find_node")
    (add-query-args result "target" target)))

(defun make-get-peers-query (info-hash)
  (with-encoded-query (result "get_peers")
    (add-query-args result "info_hash" info-hash)))

(defun make-announce-peer-query (info-hash port token &optional (implied-port nil))
  (with-encoded-query (result "announce_peer")
    (add-query-args result
                    "info_hash" info-hash
                    "port" port
                    "token" token)
    (when implied-port
      (add-query-args result "implied_port" 1))))

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

(defun error-response? (response)
  (= (elt (gethash "y" response) 0) (char-code #\e)))

(defun parse-compact-nodes-info (info-vec)
  (let ((nodes-stream (make-flexi-stream (make-in-memory-input-stream info-vec))))
    (loop until (null (peek-byte nodes-stream nil nil nil))
       collect (read-compact-node-info nodes-stream))))

(defmacro with-response-to ((response-args) query address &body body)
  "Performs query an in case of an error, returns error description
list"
  (with-gensyms (response)
    `(let* ((,response (perform-query ,query ,address))
            (,response-args (gethash "r" ,response)))
       (if (error-response? ,response)
           (gethash "e" ,response)
           (progn ,@body)))))

(defun ping (address)
  (with-response-to (response) (make-ping-query) address
    (let ((responder-id (gethash "id" response)))
      (values responder-id (byte-array-to-hex-string responder-id)))))

(defun find-node (address target-node-id)
  (with-response-to (response) (make-find-node-query target-node-id) address
    (parse-compact-nodes-info
     (gethash "nodes" response))))

(defun get-peers (address info-hash)
  (with-response-to (response) (make-get-peers-query info-hash) address
    (make-instance 'get-peers-response
                   :peer-nodes (gethash "nodes" response)
                   :peer-values (gethash "values" response)
                   :token (gethash "token" response))))

(defun announce-peer (address info-hash port token &optional implied-port)
  (with-response-to (response)
      (make-announce-peer-query info-hash port token implied-port) address
    (let ((responder-id (gethash "id" response)))
      (values responder-id (byte-array-to-hex-string responder-id)))))
