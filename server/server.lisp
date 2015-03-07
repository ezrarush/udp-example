(in-package #:udp-server)

(defvar *last-time*)
(defvar *delta-time*)

(defun start-server (server-ip port)
  (network-engine:open-server-socket server-ip port))

(defun stop-server ()
  (network-engine:close-socket))

(defun send-packet (channel buffer)
  (network-engine:send-packet channel buffer))

(defun read-packet ()
  (network-engine:receive-packets #'handle-packet-from-client))

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
	    (channel (network-engine:lookup-channel-by-port network-engine:*current-remote-port*)))
	(setf (channel client) channel)  
	(send-packet channel (make-welcome-packet (client-id client)))
	(format t "client ~a logged in~%" (client-id client))
	(finish-output)))))

(defun handle-input-packet (packet)
  (userial:with-buffer packet 
    (userial:unserialize-let* (:int32 client-id :int32 data)
			      (format t "received input data:~a from client id:~a~%" data client-id)
			      (finish-output))))

(defun handle-logout-packet (packet)
  (userial:with-buffer packet 
    (userial:unserialize-let* (:int32 client-id)
			      (assert client-id)
			      (let ((client (lookup-client-by-id client-id)))
				(remove-client client)
				(format t "client ~a: logged out~%" client-id)
				(finish-output)))))

(defun make-welcome-packet (client-id)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcode :welcome
			:int32 client-id)))

(defun make-update-data-packet (data)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcode :update-data
			:int32 data)))

(defun server-main (&key (server-ip "127.0.0.1") (port 2448))
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
	    sdl2-ffi:+sdl-major-version+
	    sdl2-ffi:+sdl-minor-version+
	    sdl2-ffi:+sdl-patchlevel+)
    (finish-output)
    (start-server server-ip port)
    (setf *last-time* (sdl2:get-ticks))
    (unwind-protect
	 (sdl2:with-event-loop (:method :poll)
	   (:idle
	    ()
	    (read-packet)
	    (setf *delta-time* (- (sdl2:get-ticks) *last-time*))
	    (when (>= *delta-time* 100/3)
	      (incf *last-time* 100/3)
	      (loop for channel being the hash-value in network-engine:*channels* do
		   (send-packet channel (make-update-data-packet (random 10))))))
	   (:quit () t))
      (stop-server))))
