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

