(in-package #:udp-example)

(defvar *server-socket* nil)
(defvar *server-buffer* (userial:make-buffer))

(userial:with-buffer *server-buffer*
  (userial:buffer-advance 8))

(defvar *client* nil)
(defvar *receive-port* nil)

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
    (ecase (userial:unserialize :client-opcode)
      (:login       (handle-login-message message)))))

(defun handle-login-message (message)
  (userial:with-buffer message
    (userial:unserialize-let* (:string name)
			      (assert (plusp (length name)))
			      (format t "~a has joined the server~%" name)
			      (finish-output))))

(defun make-data-message (opponent)
  (userial:with-buffer (userial:make-buffer)
    (userial:serialize* :server-opcode :data)))

(defun server-main (&key (server-ip usocket:*wildcard-host*) (port 2448))
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (finish-output)
    (start-server server-ip port)
    (unwind-protect
	 (multiple-value-bind (*server-buffer* size *client* *receive-port*)
	     (usocket:socket-receive *server-socket* *server-buffer* 8)
	   (format t "~A~%" *server-buffer*)
	   (setf *last-time* (sdl2:get-ticks))
	   (sdl2:with-event-loop (:method :poll)	     
	     (:idle 
	      ()
	      (setf *current-time* (sdl2:get-ticks))
	      (setf *delta-time* (- *current-time* *last-time*))
	      (when (>=  *delta-time* 3000.0)
		(incf *last-time* 3000.0)
		(format t "server to client~%")
		(send-message *server-socket* *client* *receive-port* *server-buffer*)))
	     (:quit () t)))
      (stop-server))))
