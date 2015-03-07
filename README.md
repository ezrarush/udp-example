#Description

A simple example of a UDP server and client

#How to Run

##SBCL on Windows

Install SBCL and Quicklisp (https://www.youtube.com/watch?v=VnWVu8VVDbI)

Download this repository and place it in your quicklisp\local-projects\ folder so that quicklisp can find it.  

Download my [network-engine](https://github.com/ezrarush/network-engine) repository and place it in your quicklisp\local-projects\ folder so that quicklisp can load it as a dependency.

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

###Multiple Clients

Run the following in yet another command line instance from the project folder:

```
sbcl --load run-client.lisp
```

Repeat for as many clients as desired

#References

[USocket Test Datagram](https://github.com/bsmr-common-lisp/usocket/blob/a9ad102c41d94105038195ca5bfb31d2c3d61ba5/test/test-datagram.lisp)

[Blackthorn3d](https://github.com/linuxaged/blackthorn3d/blob/645c4a264e31994cf1ac574076d671d636000f7c/src/examples/usocket/usocket.lisp)

[Short guide to UDP/IP Client/Server programming in Common Lisp using usockets](https://gist.github.com/shortsightedsid/a760e0d83a9557aaffcc)

[Game Networking](http://gafferongames.com/networking-for-game-programmers/) 

[1500 Archers on a 28.8: Network Programming in Age of Empires and Beyond](http://www.gamasutra.com/view/feature/3094/1500_archers_on_a_288_network_.php)

[Beej's Guide to Network Programming](http://beej.us/guide/bgnet/output/html/singlepage/bgnet.html)

[Fast-Paced Multiplayer](http://www.gabrielgambetta.com/fast_paced_multiplayer.html)

[Online multiplayer proof-of-concept](https://y4pp.wordpress.com/2014/06/04/online-multiplayer-proof-of-concept/)

[Core network structures for games](http://joostdevblog.blogspot.de/2014/09/core-network-structures-for-games.html)

[Source Multiplayer Networking](https://developer.valvesoftware.com/wiki/Source_Multiplayer_Networking)

[chopin-routing](https://github.com/jsmpereira/chopin-routing/blob/6374e96b15314dcec0d311bf3d5a9b822e4d2e1c/udp-server.lisp)

