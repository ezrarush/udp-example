(in-package #:udp-client)

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

(defun send-packet-to-server (buffer)
  (usocket:socket-send *server-connection*
		       buffer
		       32768))

(defun read-packet-from-server ()
  (when (usocket:wait-for-input *server-connection* :timeout 0 :ready-only t) 
    (handle-packet-from-server (usocket:socket-receive *server-connection* (make-array 32768 :element-type '(unsigned-byte 8) :fill-pointer t) nil))))

(defun handle-packet-from-server (packet)
  (userial:with-buffer packet
    (userial:buffer-rewind)
    (ecase (userial:unserialize :server-opcode)
      (:ack         (handle-ack-packet packet))
      (:update-data (handle-update-data-packet packet)))))

(defun handle-ack-packet (packet)
  (userial:with-buffer packet
    (userial:unserialize-let* (:uint32 sequence :int32 client-id)
			      (setf *client-id* client-id)
			      (format t "received packet ~a - ack with client id: ~A~%" sequence client-id)
			      (finish-output))))

(defun handle-update-data-packet (packet)
  (userial:with-buffer packet
    (userial:unserialize-let* (:uint32 sequence :int32 data)
			      (format t "received packet ~a - data: ~A~%" sequence data)
			      (finish-output))))

(defun make-login-packet (name)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :login
			:string name)))

(defun make-logout-packet ()
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :logout
			:int32 *client-id*)))

(defun client-main (&key (server-ip "127.0.0.1") (port 2448) (name "anonymous"))
  (connect-to-server server-ip port)
  (unwind-protect
       (progn
	 (format t "sending login message to server~%")
	 (finish-output)
	 (send-packet-to-server (make-login-packet name))
	 (format t "waiting for ack~%")
	 (finish-output)
	 (unwind-protect
	      (loop (read-packet-from-server))
	   (format t "logging out~%")
	   (finish-output)
	   (send-packet-to-server (make-logout-packet))))
    (disconnect-from-server)))
