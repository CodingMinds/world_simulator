World Simulator (Erlang)
========================

## Description

A simple world which allow tcp socket based agents to explore it (compare with
http://www.uffmm.org/EoIESS-TH/gclt/node14.html)

This work iss till in progress and only a realy simple alpha !

## Dependencies

* Min. Erlang R14B03

## Next steps

 * Define first user protocol draft
 * Implement {do, Action}
 * Implement first user protocol draft
 * Test navigation via telnet
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

## Usage

Before the first run (or after sourcecode modifications) you need to compile
the .erl files. This can be done by hand
```sh
$ erlc world.erl dummy.erl sserver.erl
```
or with the include makefile
```sh
$ make
```

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

## Licence
[GNU General Public License v3](http://www.gnu.org/licenses/gpl.html)
