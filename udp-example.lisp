;;;; udp-example.lisp

(declaim (optimize (debug 3) (speed 1) (safety 3)))

(in-package #:udp-example)

(defvar *last-time*)
(defvar *current-time*)
(defvar *delta-time*)
