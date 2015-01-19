#Description

A simple example of a UDP server and client

#How to Run

##SBCL on Windows

Install SBCL and Quicklisp (https://www.youtube.com/watch?v=VnWVu8VVDbI)

Download this repository and place it in your quicklisp\local-projects\ folder so that quicklisp can find it.  

###Server

Run the following in the command line from the project folder:

```
sbcl --load run-server.lisp
```

###Client 

Run the following in a second command line instance from the project folder:

```
sbcl --load run-client.lisp
```

#References

[USocket Test Datagram](https://github.com/bsmr-common-lisp/usocket/blob/a9ad102c41d94105038195ca5bfb31d2c3d61ba5/test/test-datagram.lisp)

[Blackthorn3d](https://github.com/linuxaged/blackthorn3d/blob/645c4a264e31994cf1ac574076d671d636000f7c/src/examples/usocket/usocket.lisp)

[Short guide to UDP/IP Client/Server programming in Common Lisp using usockets](https://gist.github.com/shortsightedsid/a760e0d83a9557aaffcc)

[Game Networking](http://gafferongames.com/networking-for-game-programmers/) 
