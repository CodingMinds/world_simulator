World Simulator (Erlang)
========================

## Description

A simple world which is based on the style of the WOOD1 environment described
from Wilson in "ZCS: A Zeroth Level Classifier System" (1994).
Agents can connect with TCP sockets and explore it. Objects are encoded
with the following ASCII symbols:

 * . - Free space
 * * - An Agent
 * O - A Object which can't be passed
 * F - Food

The socket connection use a plain text protocol, which is defined on the
end of this Reamde file. The movement and position values are based on the
following table (also from Wilsons WOOD1):

8 | 1 | 2  
7 | * | 3  
6 | 5 | 4

Also compare with the corresponding script http://www.uffmm.org/EoIESS-TH/gclt/node14.html  
Disclaimer: The author of the script is different from the authors of the
implementations.

## Authors

 * Michael Bittorf (Coding Minds)
 * André Freudenreich

## Dependencies

#### Server

 * Min. Erlang R14B03

#### Demos

 * Min. Scilab 5.4.0 with SOCKET library (2.0.1-1) OR
 * Min. Python 2.7

## Next steps

#### Server

 * Prepare some worlds and make them loadable via admin interface
 * Implement more options (like amount of food and energy, costs, etc)

#### Client demos

  * Handle grid size in welcome greeting
  * Handle food and other protocol stuff

## Logical Structure
<pre>
world (application)
 '-> world_sup (supervisor)
      |-> world_logging (gen_server)
      |-> world_env (gen_server)
      |-> world_ctl_sservsup (supervisor)
      |    '-> world_ctl_sserv (gen_server)
      '-> world_sservsup (supervisor)
           '-> world_sserv (gen_server)
</pre>

#### world_sup.erl
The global supervisor which initialize the whole application. First the
simulated world and then the supervisor of the socket server.

#### world_env.erl
The gen_server which represents the virtual world and holds all the logic.

#### world_sservsup.erl and world_sserv.erl
The socket server which handle incoming connections and forward them to the
simulated world world_env.erl  
Listens on the port which is defined in world.app as 'port' (default 4567)

#### world_ctl_sservsup.erl and world_ctl_sserv.erl
The socket server which handle incoming control connections and forward them
to the simulated world world_env.erl  
Listens on the port which is defined in world.app as 'ctl_port' (default 4568)

#### world_logging.erl
A simple logging server which writes the messages from client and environment
changes/action into separate files.  
The location of the files is defined in world.app as log_client, log_env and
log_info.

#### world_helper.erl
Some helper functions which mostly used in more than one module.

## Default behaviour

The system is initialized with a small 5x5 example world which shows all
possible objects.

To load your own world connect to the control port (see Example usage) and use
the command 'map'. Encode the map with the ASCII symbols described in the
section Description; to mark the beginning of a new row use the character |.
For example the default world (except the hard coded and not responding demo
agent) could be encoded with the following string:

<pre>
.....|.....|OO.|...OF|..X..|
</pre>

As you can see, the third row has only three elements. This should
demonstrate, that the environment interprets each not explicit defined section
as a blocking object. And each unknown character (e.g. the X) is interpreted
as a free cell.

Also the simulator has some configuration options which are not direct tied
to the world.

 * maximum amount of allowed agents (default: as much as possible)
 * should consumed food respwan (default: yes)
 * should consumed food respawn on the same place (default: yes)

All this options can changed on runtime if you are conencted with the control
port. The following command will set the maximum client count to 7,
activates the food respawn and let the food respawn on random places.

<pre>
options 7 true false
</pre>

The active configuration can be displayed if you use the 'options' command
without an argument.

Notice: There are more options planed. E.g. the amount of food if an agent
enters a food section, the amount of start energy of new clients, the amount
of energy client interactions cost, etc..

## Example Usage

#### Compiling

Before the first run (or after sourcecode modifications) you need to compile
the .erl files. This can be done by hand
```sh
$ erlc --make
```
or with the include makefile
```sh
$ make
```

#### Running

To start the application start an erlang shell in the directory ebin and enter
```erlang
1> application:start(world).
```

Then open a connection from another terminal and try to find the food
<pre>
$ telnet localhost 4567
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
200 welcome in this 5x5 world
move 1
203 blocked
move 5
201 success
environ
102 environ ...OOOOO
[...]
move 3
201 success
environ
102 environ OF.OOO..
move 2
202 food 75
quit
200 good bye
Connection closed by foreign host.
</pre>

#### Administration

If the server is running open a telnet connection to the control port
<pre>
$ telnet localhost 4568
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
200 Speak, friend, and ente(r)
map
100 .....
100 .....
100 OO.OO
100 ...OF
100 *....
map F...O|....|..........F
201 success
map
100 F...OOOOOOO
100 ....OOOOOOO
100 ..........F
quit
200 good bye
Connection closed by foreign host.
</pre>

## Demos

#### socket_demo.sci

A short Scilab socket connection demo.

#### user_interaction.sci

A short Scilab program which connects to the server and let user interact
with the world.

#### random_agent.py

A simple demo of a random acting agent which terminates if the food is found.  
The amount of attempts will be printed to stdout.

#### guest_ui.php

A simple demo of a php UI to show interested users what's going on. Uses the
ctrl port to get the whole environment.
  
## Protocol DRAFT

#### Sever

100 ASCII map representation  
101 world changed  
102 environ [.|O|F|*]{8}  
103 free text help replys  
104 option listings

200 welcome in this [1-9]+x[1-9]+ world  
200 good bye  
201 success  
202 food [0-9]+  
203 blocked  
204 staffed  
205 failed

300 bad argument  
301 death (not yet implemented)

400 unknown command  
403 access denied  
404 not found (not yet implemented)

500 sever made a boo boo  
501 world destroyed

#### Admin

map [ASCII_REPRESENTATION]  
options [OPTIONS ..]  
kill all (not yet implemented)  
shutdown (not yet implemented)

quit

#### Client

move [1-8]  
environ  
help [COMMAND]

quit

## Used Sources

 * Erlang documentation
 * http://learnyousomeerlang.com/ for supervisor based socket server
 * http://www.uffmm.org/ for theoretical stuff and specifications

## Licence

[GNU General Public License v3](http://www.gnu.org/licenses/gpl.html)
