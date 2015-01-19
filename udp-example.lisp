;;;; udp-example.lisp

(declaim (optimize (debug 3) (speed 1) (safety 3)))

(in-package #:udp-example)

(defparameter *server-port* 30000)
(defparameter *client-port* 30001)
(defparameter *protocol-id* 1234)
(defparameter *delta-time* 0.25)
(defparameter *send-rate* 0.25)
(defparameter *time-out* 10.0)
