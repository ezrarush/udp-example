(in-package #:udp-example)

(defun server-main ()
  (let ((server-socket (create-server 12355))
	(server-buffer (make-array 8 :element-type '(unsigned-byte 8))))

    ;; server
    (unwind-protect
	 (multiple-value-bind (server-buffer size client receive-port)
	     (usocket:socket-receive server-socket server-buffer 8)
	   (format t "~A~%" server-buffer)
	   (usocket:socket-send server-socket (reverse server-buffer) size
				:port receive-port
				:host client))
      (usocket:socket-close server-socket))))

