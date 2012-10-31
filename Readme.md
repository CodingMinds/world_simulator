World Simulator (Erlang)
========================

## Description

A simple world which allow tcp socket based agents to explore it (compare with
http://www.uffmm.org/EoIESS-TH/gclt/node14.html)

This work iss till in progress and only a realy simple alpha !

## Authors

 * Michael Bittorf (Coding Minds)
 * André Feudenreich

## Dependencies

#### Server

 * Min. Erlang R14B03

#### Scila demos

 * Min. Scilab 5.4.0 with SOCKET library (2.0.1-1)

## Next steps

 * Implement {do, Action}
 * Test navigation via telnet
 * Pimp sserver to handle admin port, too
 * Define first admin protocol draft and implement it
 * Prepare some worlds and make them loadable via admin interface

## Structure

#### world.erl
The gen_server which represents the world

#### sserver.erl
The socket server which handle incoming connections and forward them to the
simulated world world.erl

#### dummy.erl
Only a some simple shortcuts for development. This part will be removed later

## Example Usage

#### Compiling

Before the first run (or after sourcecode modifications) you need to compile
the .erl files. This can be done by hand
```sh
$ erlc world.erl dummy.erl sserver.erl
```
or with the include makefile
```sh
$ make
```

#### Running

To start the simple demo of the current state use
```erlang
1> sserver:start(4567).
```

```sh
telnet localhost 4567
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
200 welcome in this world
ehlo world
400 unknown command
quit
200 good bye
Connection closed by foreign host.
```

## Demos

#### socket_demo.sci

A short Scilab program which connects to the server and let user interact with the world

## Protocol DRAFT

#### Sever

100 map  
.......  
101 world changed  
102 environs [.|O|F|*]{8}

200 welcome in this world  
200 good bye  
201 success  
202 food N  
203 blocked  
204 staffed  
205 failed

300 bad argument  
301 death

400 unknown command  
403 access denied  
404 not found

500 sever made a boo boo  
501 world destroyed

#### Admin

status  
load filename  
kill all  
shutdown

quit

#### Client

move [1-8]  
environs

quit

## Licence
[GNU General Public License v3](http://www.gnu.org/licenses/gpl.html)
