;;;; udp-example.asd

(asdf:defsystem #:udp-example
  :serial t
  :description "A simple UDP server and client"
  :author "Ezra Rush <rushwest@gmail.com>"
  :license "The MIT License (MIT) Copyright (c) 2015 Ezra Rush"
  :depends-on (#:usocket
	       #:userial
	       #:sdl2)
  :components ((:file "package")
	       (:file "serial")
	       (:file "message")
	       (:file "server")
               (:file "client")
	       (:file "udp-example")))
