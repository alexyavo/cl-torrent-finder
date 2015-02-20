;;;; torrent-finder.lisp

(in-package #:torrent-finder)

;;; "torrent-finder" goes here. Hacks and glory await!

(defclass endpoint-address ()
  ((host :initarg :hostname
         :initform (error "must provide hostname")
         :accessor hostname)
   (port :initarg :port
         :initform (error "must provide port")
         :accessor port)))

(defmethod print-object ((address endpoint-address) stream)
  (format stream "~a:~d" (hostname address) (port address)))


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

(defparameter +my-id+ "abcdefghij0123456789")
(defparameter +default-transaction-id+ "aa")

(defparameter +router-bittorrent+
  (make-instance 'endpoint-address :hostname "router.bittorrent.com" :port 6881))

(defparameter bencode:*binary-key-p* (lambda (keys)
;                                       (print keys)
                                       (if (equal (first keys) +response-args-key+)
                                           nil
                                         t)))

(defun make-ping-query (query-id my-id)
  (let ((ping-query (make-hash-table :test 'equal)))
    (setf (gethash +transaction-id-key+ ping-query) query-id)
    (setf (gethash +message-type-key+ ping-query) +query-message+)
    (setf (gethash +query-method-key+ ping-query) "ping")
    (let ((args (make-hash-table :test 'equal)))
      (setf (gethash "id" args) my-id)
      (setf (gethash +args-key+ ping-query) args))
    (bencode:encode ping-query nil)))

(defvar pingq (make-ping-query +default-transaction-id+ +my-id+))

;; (defun send-query-to (query-bytes hostname port)
;;   (let ((socket (socket-connect hostname port :protocol :datagram)))
;;     (unwind-protect
;;         (progn
;;           (socket-send socket query-bytes (length query-bytes))
;;           (let ((response (make-array 30 :element-type '(unsigned-byte 8))))
;;             (socket-receive socket response (length response))))
;;       (socket-close socket))))

;; clozure usocket doesn't support nil args for socket-recieve

(defvar *response-buffer* (make-array +max-response-bytes+ :element-type '(unsigned-byte 8)))

(defun send-query-to (query-bytes hostname port)
  (with-connected-socket
   (socket (socket-connect hostname port :protocol :datagram))
   (socket-send socket query-bytes (length query-bytes))
   (multiple-value-bind (response-buffer response-length)
       (socket-receive socket nil +max-response-bytes+)
     (subseq response-buffer 0 response-length))))

(defun error-response? (msg-dict)
  (equal (gethash +message-type-key+ msg-dict) +error-message+))

(defun dht/ping (address)
  (let ((ping-query (make-ping-query +default-transaction-id+ +my-id+)))
    (with-connected-socket
     (query-socket (socket-connect (hostname address) (port address) :protocol :datagram))
     (socket-send query-socket ping-query +max-response-bytes+)
     (multiple-value-bind (response-buffer response-length)
         (socket-receive query-socket nil +max-response-bytes+)
       (let* ((response-msg (subseq response-buffer 0 response-length))
              (response-dict (bencode:decode response-msg)))
         (if (error-response? response-dict)
             (gethash +error-description-list-key+ response-dict)
           (gethash +id-key+ (gethash +response-args-key+ response-dict))))))))

(defun dht/find-node (address target-node-id)
  (let ((find-node-query (make-find-node-query
                          +default-transaction-id+
                          +my-id+
                          target-node-id)))))

(defun print-hash-table-entry (key value)
  (format *standard-output* "~S: ~S~%" key value))

(defun print-hash-table (hash-table)
  (with-hash-table-iterator
   (it hash-table)
   (loop (multiple-value-bind (entry? key value) (it)
           (if entry?
               (print-hash-table-entry key value)
             (return))))))
