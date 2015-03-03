(in-package #:torrent-dht)

(defclass node-info ()
  ((node-id :initarg :node-id
            :reader node-id)
   (node-address :initarg :node-address
                 :reader node-address)))

(defmethod print-object ((node node-info) stream)
  (let* ((id (node-id node))
         (id-string (if (null id)
                        "NaN"
                        (byte-array-to-hex-string id))))
    (format stream "<DHT node: ~a, ~a>" id-string (node-address node))))

(defmacro def-nodes (varname &rest args)
  (let ((nodes
         (loop for (host port id) on args by #'cdddr collect
              `(make-instance 'node-info
                              :node-id ,id
                              :node-address (make-instance 'endpoint-address
                                                          :host ,host :port ,port)))))
    `(defparameter ,varname (list ,@nodes))))

(def-nodes +well-known-nodes+
    "router.bittorrent.com"  6881 nil
    "router.utorrent.com"    6881 nil
;    "router.bitcoment.com"   6881 nil
    "dht.transmissionbt.com" 6881 nil)

(defparameter +id-length+ 20)

(defparameter +max-response-bytes+ 8196)
(defparameter *response-buffer*
  (make-array +max-response-bytes+ :element-type '(unsigned-byte 8)))

;; Convenience definitions for debugging / interacting with DHT
(defvar *default-query-id* "aa")
(defvar *default-id* "abcdefghij0123456789")

(define-condition error-response (error)
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
                 :port (read-uint16 stream)))

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

(defun error-response? (response)
  (= (elt (gethash "y" response) 0) (char-code #\e)))

(defun parse-compact-nodes-info (info-vec)
  (if (null info-vec)
      nil
      (let ((nodes-stream (make-flexi-stream (make-in-memory-input-stream info-vec))))
        (loop until (null (peek-byte nodes-stream nil nil nil))
           collect (read-compact-node-info nodes-stream)))))

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

(defun perform-query (address query)
  "Returns query response arguments"
  (let* ((response (bencoding/decode (udp/send-receive address query)))
         (response-args (gethash "r" response)))
    (if (error-response? response)
        (let ((error-desc-list (gethash "e" response)))
          (error 'error-response
                 :error-code (first error-desc-list)
                 :error-msg (octets-to-string (second error-desc-list))))
        response-args)))

(defun ping (address)
  "Returns pinged node id"
  (let ((response (perform-query address (make-ping-query))))
    (gethash "id" response)))

(defun find-node (address target-node-id)
  "Returns K closes nodes to target-node-id"
  (let ((response (perform-query address (make-find-node-query target-node-id))))
    (parse-compact-nodes-info (gethash "nodes" response))))

(defun get-peers (address info-hash)
  "Returns closes peers, peers and token"
  (let ((response (perform-query address (make-get-peers-query info-hash))))
    (make-instance 'get-peers-response
                   :peer-nodes (gethash "nodes" response)
                   :peer-values (gethash "values" response)
                   :token (gethash "token" response))))

(defun announce-peer (address info-hash port token &optional implied-port)
  (perform-query address (make-announce-peer-query info-hash port token implied-port)))

(defmacro on-query-success ((response (query-fn address &rest query-args)) &body body)
  `(handler-case
       (let ((,response (,query-fn ,address ,@query-args)))
         ,@body)
     (usocket:host-unreachable-error ()
       (log:warn "Unreachable node: ~a" ,address))
     (usocket:timeout-error ()
       (log:warn "Timeout on node: ~a" ,address))
     (usocket:connection-refused-error ()
       (log:warn "Connection refused by ~a" ,address))
     (error-response (err)
       (log:warn "Got error response: ~a" err))))

(defun get-torrent-peers (info-hash &key (max-peers 10))
  "Returns requested amount of peers that have info-hash"
  (let ((peers '())
        (nodes +well-known-nodes+))
    (while (and (> (length nodes) 0)
                (< (length peers) max-peers))
      (on-query-success (node-peers (get-peers (node-address (pop nodes)) info-hash))
        (setf peers (append peers (peer-values node-peers)))
        (setf nodes (append nodes (peer-nodes node-peers)))))
    peers))
