(in-package #:udp-example)

(defvar *server-socket* nil)
(defvar *current-remote-host*)
(defvar *current-remote-port*)

(let ((next-id 0))
  (defun get-next-sequence ()
    (incf next-id)))

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
	      (multiple-value-bind (buffer size *current-remote-host* *current-remote-port*)
		     (usocket:socket-receive *server-socket* (make-array 32768 :element-type '(unsigned-byte 8) :fill-pointer t) nil)
		   (handle-packet-from-client buffer))))

(defun send-packet (client buffer)
  (usocket:socket-send *server-socket* 
		       buffer
		       32768
		       :host (remote-host client)
		       :port (remote-port client)))

(defun handle-packet-from-client (packet)
  (userial:with-buffer packet
    (userial:buffer-rewind)
    (ecase (userial:unserialize :client-opcode)
      (:login  (handle-login-packet packet))
      (:logout (handle-logout-packet packet)))))

(defun handle-login-packet (packet) 
  (userial:with-buffer packet
    (userial:unserialize-let* (:string name)
			      (assert (plusp (length name)))
			      (let ((client (make-client name *current-remote-host* *current-remote-port*)))
				(send-packet client (make-ack-packet (client-id client)))
				(format t "client ~a has joined the server~%" (client-id client))
				(finish-output)))))

(defun handle-logout-packet (packet)
  (userial:with-buffer packet 
    (userial:unserialize-let* (:int32 client-id)
			      (assert client-id)
			      (let ((client (lookup-client-by-id client-id)))
				(remove-client client)
				(format t "client ~a logged out~%" client-id)
				(finish-output)))))

(defun make-ack-packet (client-id)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcode :ack
			:uint32 (get-next-sequence)
			:int32 client-id)))

(defun make-update-data-packet (data)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcode :update-data
			:uint32 (get-next-sequence)
			:int32 data)))

(defun server-main (&key (server-ip usocket:*wildcard-host*) (port 2448))
  (start-server server-ip port)
  (unwind-protect
       (loop 
	  (read-packet)
	  (format t "sending data to ~a client(s)~%" (hash-table-count *clients*))
	  (finish-output)
	  (let ((buffer (make-update-data-packet (random 10))))
	    (loop for client being the hash-value in *clients* do
		 (send-packet client buffer)))
	  (sleep 1))
    (stop-server)))
