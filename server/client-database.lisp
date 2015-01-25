(in-package #:udp-server)

(defvar *clients* (make-hash-table))

(let ((next-id 0))
  (defun make-client-id ()
    (incf next-id)))

(defun client-id-in-use-p (id)
  (multiple-value-bind (object exists) (gethash id *clients*)
    (declare (ignore id))
    exists))

(defun lookup-client-by-id (id)
  (multiple-value-bind (client exists) (gethash id *clients*)
    (unless exists (error "No client for id ~a." id))
    client))

(defun add-client (client)
  (let ((id (client-id client)))
    (assert (not (client-id-in-use-p id)))
    (setf (gethash id *clients*) client)))

(defun remove-client (client)
  (let ((id (client-id client)))
    (assert (eql (lookup-client-by-id id) client))
    (remhash id *clients*)))

(defclass client ()
  ((id :type integer
       :initform (make-client-id)
       :accessor client-id)
   (name 
    :initarg :name
    :accessor name)
   (remote-host
    :initarg :remote-host
    :accessor remote-host)
   (remote-port
    :initarg :remote-port
    :accessor remote-port)))

(defmethod initialize-instance :after ((self client) &key)
  (add-client self))

(defun make-client (name remote-host remote-port)
  (make-instance 'client :name name :remote-host remote-host :remote-port remote-port))

