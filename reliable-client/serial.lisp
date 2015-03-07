(in-package #:udp-reliable-client)

(userial:make-enum-serializer :client-opcode
                      (:login :input :logout))

(userial:make-enum-serializer :server-opcode
                      (:welcome :update-data))
