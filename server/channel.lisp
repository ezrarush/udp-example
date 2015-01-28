(in-package #:udp-server)

(defvar *channels* (make-hash-table))

(let ((next-id 0))
  (defun make-channel-id ()
    (incf next-id)))

(defun channel-id-in-use-p (id)
  (multiple-value-bind (object exists) (gethash id *channels*)
    (declare (ignore id))
    exists))

(defun lookup-channel-by-port (port)
  (let ((found nil))
    (loop for channel being the hash-value in *channels* do
	 (when (eq (remote-port channel) port)
	   (setf found channel)))
    found))

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
    :type (unsigned-byte 32)
    :initform 0
    :accessor ack-bitfield)
   (sent-packets
    :initform (list)
    :accessor sent-packets)
   (received-packets
    :initform (list)
    :accessor received-packets)
   (pending-ack-packets
    :initform (list)
    :accessor pending-ack-packets)
   (acked-packets
    :initform (list)
    :accessor acked-packets)
   (number-sent
    :initform 0)
   (number-received
    :initform 0)
   (number-lost
    :initform 0)
   (number-acked
    :initform 0)
   (sent-bandwidth
    :initform 0)
   (acked-bandwidth
    :initform 0)
   (rtt
    :initform 0)))

(defmethod initialize-instance :after ((self channel) &key)
  (add-channel self))

(defmethod send-packet ((self channel) buffer)
  (with-slots (remote-host remote-port local-sequence-number sent-packets pending-ack-packets number-sent) self
    (usocket:socket-send *server-socket* 
			 buffer
			 32768
			 :host remote-host
			 :port remote-port)
    (let ((data (make-packet-data :sequence local-sequence-number :time 0 :size 0)))
      (setf sent-packets (append sent-packets (list data)))
      (setf pending-ack-packets (append pending-ack-packets (list data))))
    (incf number-sent))
  (next-sequence-number self))

(defmethod next-sequence-number ((self channel))
  (with-slots (local-sequence-number) self
    (incf local-sequence-number)))

(defmethod update-channel ((self channel) sequence)
  (with-slots (number-received received-packets remote-sequence-number) self
    (incf number-received)
    (let ((data (make-packet-data :sequence sequence :time 0 :size 0)))
      (setf received-packets (append received-packets (list data))))
    (when (> sequenc remote-sequence-number)
      (setf remote-sequence-number sequence))))


(defun make-channel (remote-host remote-port)
  (make-instance 'channel :remote-host remote-host :remote-port remote-port))

(defstruct packet-data
  sequence
  time
  size)
