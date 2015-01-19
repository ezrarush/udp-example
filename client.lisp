(in-package #:udp-example)

(defvar *server-connection* nil)
(defvar *client-buffer* (make-array 8 :element-type '(unsigned-byte 8)))

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

(defun client-main (&key (server-ip "127.0.0.1") (port 2448))
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (finish-output)

    (connect-to-server server-ip port)

    (unwind-protect
	 (progn
	   (replace *client-buffer* #(1 2 3 4 5 6 7 8))
	   (format t "Sending data~%")
	   (usocket:socket-send *server-connection* *client-buffer* 8)
	   (format t "Receiving data~%")
	   (usocket:socket-receive *server-connection* *client-buffer* 8)
	   (format t "~A~%" *client-buffer*))
	 
	 (sdl2:with-event-loop (:method :poll)	     
	   (:idle 
	    ()

	    )
	   (:quit () t))
      (disconnect-from-server))))
