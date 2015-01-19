(in-package #:udp-example)



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

