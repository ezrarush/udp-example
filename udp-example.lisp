;;;; udp-example.lisp

(in-package #:udp-example)

(defun create-server (port)
  (usocket:socket-connect nil nil
			  :protocol :datagram
			  :element-type '(unsigned-byte 8)
			  :local-host "127.0.0.1"
			  :local-port port))

(defun create-client (port)
  (usocket:socket-connect "127.0.0.1" port
			  :protocol :datagram
			  :element-type '(unsigned-byte 8)))

;; (defun send-packet (to buffer)
;;   (unwind-protect
;; 	 (multiple-value-bind (buffer size client receive-port)
;; 	     (usocket:socket-receive socket buffer 8)
;; 	   (format t "~A~%" buffer)
;; 	   (usocket:socket-send socket (reverse buffer) size
;; 				:port receive-port
;; 				:host client))
;;       (usocket:socket-close socket)))

;; (defun receive-packet (buffer)
;;   (unwind-protect
;; 	 (multiple-value-bind (buffer size client receive-port)
;; 	     (usocket:socket-receive socket buffer 8)
;; 	   (format t "~A~%" buffer))
;;       (usocket:socket-close socket)))

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
