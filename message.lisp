(in-package #:udp-example)

(defun send-message (socket host port buffer)
  (userial:with-buffer buffer
    (usocket:socket-send socket
			 buffer
			 (userial:buffer-length)
			 :host host
			 :port port)))

;; (defun read-message (connection)
  
;;   (multiple-value-bind (*server-buffer* size *client* *receive-port*)
;;       (usocket:socket-receive *server-socket* nil nil)))

;; (defun read-message (connection)
;;   (let* ((buffer (userial:make-buffer))
;; 	 (stream (usocket:socket-stream connection)))

;;     ;; Read the size of the message in bytes, then read those bytes
;;     (when (listen stream)
;;       (userial:with-buffer buffer
;; 	(let* ((size (read-byte stream)))
;; 	  (userial:buffer-advance size)
;; 	  (read-sequence buffer stream :end size))

;; 	(unless (zerop (userial:buffer-length))
;; 	  (userial:buffer-rewind)
;; 	  (handle-message-from-client buffer))))))
