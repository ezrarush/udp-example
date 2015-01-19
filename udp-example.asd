;;;; udp-example.asd

(asdf:defsystem #:udp-example
  :serial t
  :description "Describe udp-example here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:usocket)
  :components ((:file "package")
               (:file "udp-example")))

