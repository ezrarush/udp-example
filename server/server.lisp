(in-package #:udp-server)

(defvar *server-socket* nil)
(defvar *current-remote-host*)
(defvar *current-remote-port*)

(defvar *last-time*)
(defvar *delta-time*)

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
  (loop until (not (usocket:wait-for-input *server-socket* :timeout 0 :ready-only t)) do
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
	(send-packet channel (make-welcome-packet (sequence-number channel) (remote-sequence-number channel) (generate-ack-bits channel) (client-id client)))
	(format t "client ~a logged in~%" (client-id client))
	(finish-output)))))

(defun handle-input-packet (packet)
  (userial:with-buffer packet 
    (userial:unserialize-let* (:uint32 sequence :uint32 ack :uint32 ack-bitfield)
			      (receive-packet (lookup-channel-by-port *current-remote-port*) sequence ack ack-bitfield))))

(defun handle-logout-packet (packet)
  (userial:with-buffer packet 
    (userial:unserialize-let* (:uint32 sequence :uint32 ack  :uint32 ack-bitfield :int32 client-id)
			      (receive-packet (lookup-channel-by-port *current-remote-port*) sequence ack ack-bitfield)
			      (assert client-id)
			      (let ((client (lookup-client-by-id client-id)))
				(remove-client client)
				(format t "client ~a: logged out~%" client-id)
				(finish-output)))))

(defun make-welcome-packet (sequence ack ack-bitfield client-id)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcode :welcome
			:uint32 sequence
			:uint32 ack
			:uint32 ack-bitfield
			:int32 client-id)))

(defun make-update-data-packet (sequence ack ack-bitfield data)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcode :update-data
			:uint32 sequence
			:uint32 ack
			:uint32 ack-bitfield
			:int32 data)))

(defun server-main (&key (server-ip usocket:*wildcard-host*) (port 2448))
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
	      (loop for channel being the hash-value in *channels* do
		   (send-packet channel (make-update-data-packet (sequence-number channel) (remote-sequence-number channel) (generate-ack-bits channel) (random 10)))
		   (update channel))))
	   (:quit () t))
      (stop-server))))
