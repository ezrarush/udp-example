(in-package #:udp-example)

(defun send-message (socket host port buffer)
  (userial:with-buffer buffer
    (usocket:socket-send socket
			 buffer
			 (userial:buffer-length)
			 :host host
			 :port port)))

(defun read-message ()
  (usocket:socket-receive *server-socket* *server-buffer* 128))
