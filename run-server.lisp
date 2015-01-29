(ql:quickload "udp-example")
( ql:quickload "swank")
(bt:make-thread (lambda () (swank:create-server :port 4005 :dont-close t)))
(udp-server:server-main)
