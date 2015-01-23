(in-package #:udp-example)

(defvar *server-socket* nil)
(defvar *current-remote-host*)
(defvar *current-remote-port*)

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

(defun read-message ()
  (when (usocket:wait-for-input *server-socket* :timeout 0 :ready-only t) 
	      (multiple-value-bind (buffer size *current-remote-host* *current-remote-port*)
		     (usocket:socket-receive *server-socket* (make-array 32768 :element-type '(unsigned-byte 8) :fill-pointer t) nil)
		   (handle-message-from-client buffer))))

(defun send-message (client buffer)
  (usocket:socket-send *server-socket* 
		       buffer
		       32768
		       :host (remote-host client)
		       :port (remote-port client)))

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
			      (let ((client (make-client name *current-remote-host* *current-remote-port*)))
				(send-message client (make-ack-message (client-id client)))
				(format t "client ~a has joined the server~%" (client-id client))
				(finish-output)))))

(defun handle-logout-message (message)
  (userial:with-buffer message 
    (userial:unserialize-let* (:int32 client-id)
			      (assert client-id)
			      (let ((client (lookup-client-by-id client-id)))
				(remove-client client)
				(format t "client ~a logged out~%" client-id)
				(finish-output)))))

(defun make-ack-message (client-id)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcode :ack
			:int32 client-id)))

(defun make-update-data-message (data)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcode :update-data
			:int32 data)))

(defun server-main (&key (server-ip usocket:*wildcard-host*) (port 2448))
  (start-server server-ip port)
  (unwind-protect
       (loop 
	  (read-message)
	  (format t "sending data to ~a client(s)~%" (hash-table-count *clients*))
	  (finish-output)
	  (let ((buffer (make-update-data-message (random 10))))
	    (loop for client being the hash-value in *clients* do
		 (send-message client buffer)))
	  (sleep 1))
    (stop-server)))
