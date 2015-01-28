(in-package #:udp-server)

(defvar *server-socket* nil)
(defvar *current-remote-host*)
(defvar *current-remote-port*)

(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

(defun update-swank ()
  (continuable
    (let ((connection (or swank::*emacs-connection*
			  (swank::default-connection))))
      (when connection
	(swank::handle-requests connection t)))))

(defun start-server (server-ip port)
  (assert (not *server-socket*))
  (setf *server-socket*
	(usocket:socket-connect nil 
				nil
				:protocol :datagram
				:element-type '(unsigned-byte 8)
				:local-host server-ip
				:local-port port)))

(defun stop-server ()
  (assert *server-socket*)
  (usocket:socket-close *server-socket*)
  (setf *server-socket* nil))

(defun read-packet ()
  (when (usocket:wait-for-input *server-socket* :timeout 0 :ready-only t) 
    (multiple-value-bind (buffer size remote-host remote-port)
	(usocket:socket-receive *server-socket* (make-array 32768 :element-type '(unsigned-byte 8) :fill-pointer t) nil)
      (setf *current-remote-host* remote-host)
      (setf *current-remote-port* remote-port)
      (handle-packet-from-client buffer))))

(defun handle-packet-from-client (packet)
  (userial:with-buffer packet
    (userial:buffer-rewind)
    (ecase (userial:unserialize :client-opcode)
      (:login  (handle-login-packet packet))
      (:input  (handle-input-packet packet))
      (:logout (handle-logout-packet packet)))))

(defun handle-login-packet (packet) 
  (userial:with-buffer packet
    (userial:unserialize-let* (:string name)
      (assert (plusp (length name)))
      (let ((client (make-client name))
	    (channel (make-channel *current-remote-host* *current-remote-port*)))
	(setf (channel client) channel)
	(send-packet channel (make-welcome-packet (sequence-number channel) (remote-sequence-number channel) (client-id client)))
	(format t "client ~a logged in~%" (client-id client))
	(finish-output)))))

(defun handle-input-packet (packet)
  (userial:with-buffer packet 
    (userial:unserialize-let* (:uint32 sequence :uint32 ack)
			      (update-channel (lookup-channel-by-port *current-remote-port*) sequence))))

(defun handle-logout-packet (packet)
  (userial:with-buffer packet 
    (userial:unserialize-let* (:uint32 sequence :uint32 ack :int32 client-id)
			      (update-channel (lookup-channel-by-port *current-remote-port*) sequence)
			      (assert client-id)
			      (let ((client (lookup-client-by-id client-id)))
				(remove-client client)
				(format t "client ~a: logged out~%" client-id)
				(finish-output)))))

(defun make-welcome-packet (sequence ack client-id)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcode :welcome
			:uint32 sequence
			:uint32 ack
			:int32 client-id)))

(defun make-update-data-packet (sequence ack data)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcode :update-data
			:uint32 sequence
			:uint32 ack
			:int32 data)))

(defun server-main (&key (server-ip usocket:*wildcard-host*) (port 2448))
  (start-server server-ip port)
  (unwind-protect
       (loop 
	  (read-packet)
	  ;; (format t "sending data to ~a client(s)~%" (hash-table-count *clients*))
	  (finish-output)
	  (loop for channel being the hash-value in *channels* do
	       (send-packet channel (make-update-data-packet (sequence-number channel) (remote-sequence-number channel) (random 10))))
	  (update-swank)
	  (sleep 1))
    (stop-server)))
