(in-package #:udp-example)

(defvar *server-connection* nil)
(defvar *client-id*)

(defun connect-to-server (server-ip port)
  (assert (not *server-connection*))
  (setf *server-connection* 
	(usocket:socket-connect server-ip
				port
				:protocol :datagram
				:element-type '(unsigned-byte 8))))

(defun disconnect-from-server ()
  (assert *server-connection*)
  (usocket:socket-close *server-connection*)
  (setf *server-connection* nil))

(defun send-message-to-server (buffer)
  (usocket:socket-send *server-connection*
		       buffer
		       32768))

(defun read-message-from-server ()
  (when (usocket:wait-for-input *server-connection* :timeout 0 :ready-only t) 
    (handle-message-from-server (usocket:socket-receive *server-connection* (make-array 32768 :element-type '(unsigned-byte 8) :fill-pointer t) nil))))

(defun handle-message-from-server (message)
  (userial:with-buffer message
    (userial:buffer-rewind)
    (ecase (userial:unserialize :server-opcode)
      (:ack         (handle-ack-message message))
      (:update-data (handle-update-data-message message)))))


(defun handle-ack-message (message)
  (userial:with-buffer message
    (userial:unserialize-let* (:int32 client-id)
			      (setf *client-id* client-id)
			      (format t "received ack with client id: ~A~%" client-id)
			      (finish-output))))

(defun handle-update-data-message (message)
  (userial:with-buffer message
    (userial:unserialize-let* (:int32 data)
			      (format t "received data: ~A~%" data)
			      (finish-output))))

(defun make-login-message (name)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :login
			:string name)))

(defun make-logout-message ()
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :logout
			:int32 *client-id*)))

(defun client-main (&key (server-ip "127.0.0.1") (port 2448) (name "anonymous"))
  (connect-to-server server-ip port)
  (unwind-protect
       (progn
	 (format t "sending login message to server~%")
	 (finish-output)
	 (send-message-to-server (make-login-message name))
	 (format t "waiting for ack~%")
	 (finish-output)
	 (unwind-protect
	      (loop (read-message-from-server))
	   (format t "logging out~%")
	   (finish-output)
	   (send-message-to-server (make-logout-message))))
    (disconnect-from-server)))
