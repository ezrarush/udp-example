(in-package #:udp-client)

(defvar *server-connection* nil)
(defvar *client-id* nil)

(let ((next-id 0))
  (defun get-next-sequence ()
    (incf next-id)))

;; sequence number of the most recently received packet
(defvar *remote-sequence-number* 0)

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
		       (length buffer)))

(defun read-packet-from-server ()
  (loop until (not (usocket:wait-for-input *server-connection* :timeout 0 :ready-only t)) do 
    (handle-packet-from-server (usocket:socket-receive *server-connection* (make-array 32768 :element-type '(unsigned-byte 8) :fill-pointer t) nil))))

(defun handle-packet-from-server (packet)
  (userial:with-buffer packet
    (userial:buffer-rewind)
    (ecase (userial:unserialize :server-opcode)
      (:welcome     (handle-welcome-packet packet))
      (:update-data (handle-update-data-packet packet)))))

(defun handle-welcome-packet (packet)
  (userial:with-buffer packet
    (userial:unserialize-let* (:uint32 sequence :uint32 ack :int32 client-id)
			      (assert (> sequence *remote-sequence-number*))
			      (setf *remote-sequence-number* sequence)
			      (setf *client-id* client-id)
			      (format t "received packet ~a - ack with client id: ~A~%" sequence client-id)
			      (finish-output))))

(defun handle-update-data-packet (packet)
  (userial:with-buffer packet
    (userial:unserialize-let* (:uint32 sequence :uint32 ack :int32 data)
			      (assert (> sequence *remote-sequence-number*))
			      (setf *remote-sequence-number* sequence)
			      (format t "received packet ~a - data: ~A~%" sequence data)
			      (finish-output))))

(defun make-login-packet (name)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :login
			:string name)))

(defun make-input-packet ()
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :input
			:uint32 (get-next-sequence)
			:uint32 *remote-sequence-number*)))

(defun make-logout-packet ()
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :client-opcode :logout
			:uint32 (get-next-sequence)
			:uint32 *remote-sequence-number*
			:int32 *client-id*)))

(defun client-main (&key (server-ip "127.0.0.1") (port 2448) (name "anonymous"))
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
	    sdl2-ffi:+sdl-major-version+
	    sdl2-ffi:+sdl-minor-version+
	    sdl2-ffi:+sdl-patchlevel+)
    (finish-output)
    (connect-to-server server-ip port)
    (unwind-protect
	 (progn
	   (format t "sending login message to server~%")
	   (finish-output)
	   (send-packet-to-server (make-login-packet name))
	   (format t "waiting for ack~%")
	   (finish-output)
	   (unwind-protect
		(sdl2:with-event-loop (:method :poll)
		  (:idle
		   ()
		   (read-packet-from-server)
		   (when *client-id* (send-packet-to-server (make-input-packet)))
		   (sleep 1/3)
		   )
		  (:quit () t))	   
	     (format t "logging out~%")
	     (finish-output)
	     (send-packet-to-server (make-logout-packet))))
      (disconnect-from-server))))
