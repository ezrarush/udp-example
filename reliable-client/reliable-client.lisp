(in-package #:udp-reliable-client)

(defvar *channel*)
(defvar *client-id* nil)

(defvar *last-time*)
(defvar *delta-time*)

(defun connect-to-server (server-ip port)
  (network-engine:open-client-socket server-ip port)
  (setf *channel* (network-engine:make-reliable-channel server-ip port)))

(defun disconnect-from-server ()
  (network-engine:close-socket))

(defun send-packet (buffer)
  (network-engine:send-packet *channel* buffer))

(defun read-packet ()
  (network-engine:receive-reliable-packets #'handle-packet-from-server))

(defun handle-packet-from-server (packet)
  (userial:with-buffer packet
    (ecase (userial:unserialize :server-opcode)
      (:welcome     (handle-welcome-packet packet))
      (:update-data (handle-update-data-packet packet)))))

(defun handle-welcome-packet (packet)
  (userial:with-buffer packet
    (userial:unserialize-let* (:int32 client-id)
			      (setf *client-id* client-id)
			      (format t "received client id: ~A~%" client-id)
			      (finish-output))))

(defun handle-update-data-packet (packet)
  (userial:with-buffer packet
    (userial:unserialize-let* (:int32 data)
			      (format t "received data: ~A~%" data)
			      (finish-output))))

(defun make-login-packet (name)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :uint32 0
			:uint32 0
			:uint32 0
			:client-opcode :login
			:string name)))

(defun make-input-packet (sequence ack ack-bitfield)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :uint32 sequence
			:uint32 ack
			:uint32 ack-bitfield
			:client-opcode :input)))

(defun make-logout-packet (sequence ack ack-bitfield)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :uint32 sequence
			:uint32 ack
			:uint32 ack-bitfield
			:client-opcode :logout
			:int32 *client-id*)))

(defun main (&key (server-ip "127.0.0.1") (port 2448) (name "anonymous"))
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
	    sdl2-ffi:+sdl-major-version+
	    sdl2-ffi:+sdl-minor-version+
	    sdl2-ffi:+sdl-patchlevel+)
    (finish-output)
    (connect-to-server server-ip port)
    (setf *last-time* (sdl2:get-ticks))
    (unwind-protect
	 (progn
	   (format t "sending login message to server~%")
	   (finish-output)
	   (send-packet (make-login-packet name))
	   (format t "waiting for ack~%")
	   (finish-output)
	   (unwind-protect
		(sdl2:with-event-loop (:method :poll)
		  (:idle
		   ()
		   (read-packet)
		   (setf *delta-time* (- (sdl2:get-ticks) *last-time*))
		   (when (>= *delta-time* 100)
		     (incf *last-time* *delta-time*)
		     (when *client-id* (send-packet (make-input-packet (network-engine:sequence-number *channel*) (network-engine:remote-sequence-number *channel*) (network-engine:generate-ack-bitfield *channel*))))
		     (network-engine:channel-update *channel* *delta-time*)))
		  (:quit () t))	   
	     (format t "logging out~%")
	     (finish-output)
	     (send-packet (make-logout-packet (network-engine:sequence-number *channel*) (network-engine:remote-sequence-number *channel*) (network-engine:generate-ack-bitfield *channel*)))))
      (disconnect-from-server))))
