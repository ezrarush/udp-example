(in-package #:udp-example)

(defun client-main ()
  (let ((client-socket (create-client 12355))
	(client-buffer (make-array 8 :element-type '(unsigned-byte 8))))

    ;; client
    (unwind-protect
	 (progn
	   (replace client-buffer #(1 2 3 4 5 6 7 8))
	   (format t "Sending data~%")
	   (usocket:socket-send client-socket client-buffer 8)
	   (format t "Receiving data~%")
	   (usocket:socket-receive client-socket client-buffer 8)
	   (format t "~A~%" client-buffer))
      (usocket:socket-close client-socket))))

