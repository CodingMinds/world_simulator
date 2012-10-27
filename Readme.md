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
 * Implement skeleton of socket server
 * Implement first user protocol draft
 * Test navigation via telnet
 * Define first admin protocol draft and implement it
 * Prepare some worlds and make them loadable via admin interface

## Structure

#### world.erl
The gen_server which represents the world

#### dummy.erl
Only a some simple shortcuts for development. This part will be removed later

## Usage

Before the first run (or after sourcecode modifications) you need to compile the
.erl files. This can be done by hand
```sh
$ erlc world.erl dummy.erl
```
or with the include makefile
```sh
$ make
```

To start the simple demo of the current state use
```erlang
1> dummy:world_start().
{ok, ..}
2> dumm:world_state().
{...}
3> dummy:world_stop().
ok.
```

## Licence
[GNU General Public License v3](http://www.gnu.org/licenses/gpl.html)
