(in-package #:udp-reliable-server)

(defvar *last-time*)
(defvar *delta-time*)

(defun start-server (server-ip port)
  (network-engine:open-server-socket server-ip port))

(defun stop-server ()
  (network-engine:close-socket))

(defun send-packet (channel buffer)
  (network-engine:send-packet channel buffer))

(defun read-packets ()
  (network-engine:receive-packets #'handle-packet-from-client))

(defun handle-packet-from-client (packet)
  (userial:with-buffer packet
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
	(send-packet channel (make-welcome-packet (network-engine:sequence-number channel) (network-engine:remote-sequence-number channel) (network-engine:generate-ack-bitfield channel) (client-id client)))
	(format t "client ~a logged in~%" (client-id client))
	(finish-output)))))

(defun handle-input-packet (packet)
  (userial:with-buffer packet 
    (userial:unserialize-let* ()

			      )))

(defun handle-logout-packet (packet)
  (userial:with-buffer packet 
    (userial:unserialize-let* (:int32 client-id)
			      (assert client-id)
			      (let ((client (lookup-client-by-id client-id)))
				(remove-client client)
				(format t "client ~a: logged out~%" client-id)
				(finish-output)))))

(defun make-welcome-packet (sequence ack ack-bitfield client-id)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :uint32 sequence
			:uint32 ack
			:uint32 ack-bitfield
			:server-opcode :welcome
			:int32 client-id)))

(defun make-update-data-packet (sequence ack ack-bitfield data)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :uint32 sequence
			:uint32 ack
			:uint32 ack-bitfield
			:server-opcode :update-data
			:int32 data)))

(defun main (&key (server-ip "127.0.0.1") (port 2448))
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
	    sdl2-ffi:+sdl-major-version+
	    sdl2-ffi:+sdl-minor-version+
	    sdl2-ffi:+sdl-patchlevel+)
    (finish-output)
    (start-server server-ip port)
    (setf *last-time* (sdl2:get-ticks))
    (unwind-protect
	 (let ((stats-accumulator 0))
	   (sdl2:with-event-loop (:method :poll)
	     (:idle
	      ()
	      (read-packets)
	      (setf *delta-time* (- (sdl2:get-ticks) *last-time*))
	      (when (>= *delta-time* 100/3)
		(incf *last-time* *delta-time*)
		(incf stats-accumulator *delta-time*)
		(loop for channel being the hash-value in network-engine:*channels* do
		     (send-packet channel (make-update-data-packet (network-engine:sequence-number channel) (network-engine:remote-sequence-number channel) (network-engine:generate-ack-bitfield channel) (random 10)))
		     (network-engine:channel-update channel *delta-time*)
		     (when (>= stats-accumulator 5000)
		       (format t "rtt ~ams, sent:~a, acked:~a, lost:~a(~a), sent bandwidth ~akbps, acked bandwidth ~akkps~%"
			       (network-engine:rtt channel)
			       (network-engine:number-sent channel)
			       (network-engine:number-acked channel)
			       (network-engine:number-lost channel)
			       (if (> (network-engine:number-sent channel) 0) 0 0)
			       (network-engine:bandwidth-sent channel)
			       (network-engine:bandwidth-acked channel))))

		(when (>= stats-accumulator 5000)
		  (setf stats-accumulator 0))))
	     (:quit () t)))
      (stop-server))))
