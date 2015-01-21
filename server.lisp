(in-package #:udp-example)

(defvar *server-socket* nil)

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

(defun handle-message-from-client (message)
  (userial:with-buffer message
    (userial:buffer-rewind)
    (ecase (userial:unserialize :client-opcode)
      (:login  (handle-login-message message))
      (:logout (handle-logout-message message)))))

(defun handle-login-message (message) 
  (userial:with-buffer message
    (userial:unserialize-let* (:string name)
			      (assert (plusp (length name)))
			      (format t "~a has joined the server~%" name)
			      (finish-output))))

(defun handle-logout-message (message)
  (userial:with-buffer message 
	 (format t "client logged out~%")))

(defun make-update-data-message (data)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcode :update-data
			:int32 data)))

(defun server-main (&key (server-ip usocket:*wildcard-host*) (port 2448))
  (start-server server-ip port)
  (unwind-protect
       (progn (multiple-value-bind (buffer size client receive-port)
		  (usocket:socket-receive *server-socket* (make-array 32768 :element-type '(unsigned-byte 8) :fill-pointer t) nil)
		(handle-message-from-client buffer)
		(format t "sending random data to client~%")
		(usocket:socket-send *server-socket* 
				     (make-update-data-message (random 10))
				     32768
				     :port receive-port
				     :host client))
	      (multiple-value-bind (buffer size client receive-port )
	      	  (usocket:socket-receive *server-socket* (make-array 32768 :element-type '(unsigned-byte 8) :fill-pointer t) nil)
	      	(handle-message-from-client buffer )))
    (stop-server)))
