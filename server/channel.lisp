(in-package #:udp-server)

;; local sequence number
(let ((next-id 0))
  (defun get-next-sequence ()
    (incf next-id)))

(defvar *channels* (make-hash-table))

(let ((next-id 0))
  (defun make-channel-id ()
    (incf next-id)))

(defun channel-id-in-use-p (id)
  (multiple-value-bind (object exists) (gethash id *channels*)
    (declare (ignore id))
    exists))

(defun lookup-channel-by-id (id)
  (multiple-value-bind (channel exists) (gethash id *channels*)
    (unless exists (error "No channel for id ~a." id))
    channel))

(defun add-channel (channel)
  (let ((id (channel-id channel)))
    (assert (not (channel-id-in-use-p id)))
    (setf (gethash id *channels*) channel)))

(defun remove-channel (channel)
  (let ((id (channel-id channel)))
    (assert (eql (lookup-channel-by-id id) channel))
    (remhash id *channels*)))

(defclass channel ()
  ((id :type integer
       :initform (make-channel-id)
       :accessor channel-id)
   (remote-host
    :initarg :remote-host
    :accessor remote-host)
   (remote-port
    :initarg :remote-port
    :accessor remote-port)
   (local-sequence-number
    :initform (get-next-sequence)
    :accessor sequence-number)
   (remote-sequence-number
    :initform 0
    :accessor remote-sequence-number)))

(defmethod initialize-instance :after ((self channel) &key)
  (add-channel self))

(defun make-channel (remote-host remote-port)
  (make-instance 'channel :remote-host remote-host :remote-port remote-port))
