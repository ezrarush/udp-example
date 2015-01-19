(in-package #:udp-example)

(defvar *server-socket* nil)
(defvar *server-buffer* (make-array 8 :element-type '(unsigned-byte 8)))

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

(defun server-main (&key (server-ip usocket:*wildcard-host*) (port 2448))
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (finish-output)
    (start-server server-ip port)
    (unwind-protect
	 (multiple-value-bind (*server-buffer* size client receive-port)
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
		(usocket:socket-send *server-socket* (reverse *server-buffer*) size
				     :port receive-port
				     :host client))
	      )
	     (:quit () t)))
      (stop-server))))
