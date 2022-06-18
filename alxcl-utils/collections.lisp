(in-package :alxcl-utils)

(defgeneric add-item (coll item))
(defgeneric add-items (coll items))
(defgeneric remove-item (coll item))
(defgeneric get-items (coll))
(defgeneric items-count (coll))
(defgeneric pop-item (coll))

(defclass set-collection ()
  ((dict :initarg :dict :accessor dict)))

(defmethod print-object ((coll set-collection) stream)
  (format stream "<Set Collection: ~d items>" (items-count coll)))

(defun make-set-collection (&key (test 'equalp) (initial-items nil))
  (let ((result (make-instance 'set-collection :dict (make-hash-table :test test))))
    (loop :for item :in initial-items :do
       (add-item result item))
    result))

(defmethod add-item ((coll set-collection) item)
  (setf (gethash item (dict coll)) t)
  coll)

(defmethod add-items ((coll set-collection) items)
  (loop :for item :in items :do (add-item coll item)))

(defmethod remove-item ((coll set-collection) item)
  (remhash item (dict coll))
  coll)

(defmethod get-items ((coll set-collection))
  (loop :for item :being :the :hash-keys :of (dict coll) collect item))

(defmethod items-count ((coll set-collection))
  (hash-table-count (dict coll)))

(defmethod pop-item ((coll set-collection))
  (let* ((items (get-items coll))
         (item (first items)))
    (when (not (null item))
      (remove-item coll item)
      item)))
