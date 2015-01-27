(in-package #:udp-server)

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
    :initform 1
    :accessor sequence-number)
   (remote-sequence-number
    :initform 0
    :accessor remote-sequence-number)
   (ack-bitfield
    :type unsigned-byte 32
    :initform 0
    :accessor ack-bitfield)
   (sent-packets
    :initform (make-hash-table)
    :accessor sent-packets)
   (received-packets
    :initform (make-hash-table)
    :accessor received-packets)
   (pending-ack-packets
    :initform (make-hash-table)
    :accessor pending-ack-packets)
   (acked-packets
    :initform (make-hash-table)
    :accessor acked-packets)
   (number-sent)
   (number-received)
   (number-lost)
   (number-acked)
   (sent-bandwidth)
   (acked-bandwidth)
   (rtt)))

(defmethod initialize-instance :after ((self channel) &key)
  (add-channel self))

(defmethod next-sequence-number ((self channel))
  (with-slots (local-sequence-number) self
    (incf local-sequence-number)))

(defun make-channel (remote-host remote-port)
  (make-instance 'channel :remote-host remote-host :remote-port remote-port))
