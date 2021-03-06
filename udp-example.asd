;;;; udp-client.asd

(asdf:defsystem #:udp-example
  :serial t
  :description "A simple UDP server and client"
  :author "Ezra Rush <rushwest@gmail.com>"
  :license "The MIT License (MIT) Copyright (c) 2015 Ezra Rush"
  :depends-on (#:userial
	       #:sdl2
	       #:network-engine)
  :components ((:module client
			:components ((:file "package")
				     (:file "serial")
				     (:file "client")))
	       (:module server
			:components ((:file "package")
				     (:file "serial")
				     (:file "client-database")
				     (:file "server")))
	       (:module reliable-client
			:components ((:file "package")
				     (:file "serial")
				     (:file "reliable-client")))
	       (:module reliable-server
			:components ((:file "package")
				     (:file "serial")
				     (:file "client-database")
				     (:file "reliable-server")))))
