(in-package #:udp-client)

(userial:make-enum-serializer :client-opcode
                      (:login :logout))

(userial:make-enum-serializer :server-opcode
                      (:ack :update-data))
