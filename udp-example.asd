;;;; udp-example.asd

(asdf:defsystem #:udp-example
  :serial t
  :description "A simple UDP server and client"
  :author "Ezra Rush <rushwest@gmail.com>"
  :license "The MIT License (MIT) Copyright (c) 2015 Ezra Rush"
  :depends-on (#:usocket
	       #:sdl2)
  :components ((:file "package")
	       (:file "udp-example")
	       (:file "network")
	       (:file "server")
               (:file "client")))
