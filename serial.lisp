(in-package #:udp-example)

(userial:make-enum-serializer :client-opcode
                      (:login :logout))

(userial:make-enum-serializer :server-opcode
                      (:match-begin :data))
