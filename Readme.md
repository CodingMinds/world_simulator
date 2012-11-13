World Simulator (Erlang)
========================

## Description

A system which provides some simple worlds which are based on the style
of the WOOD1 environment described from Wilson in "ZCS: A Zeroth Level
Classifier System" (1994). Agents can connect with TCP sockets to these
worlds and explore them. Objects are encode with the following ASCII
symbols:

 * . - Free space
 * * - An Agent
 * O - A Object which can't be passed
 * F - Food

The socket connection use a plain text protocol, which is defined on the
end of this Reamde file. The movement and position values are based on the
following table where 0 is the actual position if the agent (also from
Wilsons WOOD1):

8 | 1 | 2  
7 | 0 | 3  
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
 * Min. Python 2.7 OR
 * Min. Common Lisp 2.49 with usocket (0.5.5)

## Next steps

#### Server

 * Implement handling of multiple worlds/environments

#### Client demos

  * Implement handling of multiple worlds/environments
  * Improve demo clients and build more

## Logical Structure
<pre>
world (application)
 '-> world_sup (supervisor)
      |-> world_ofo_sup (supervisor)
      |    |-> world_logging (gen_server)
      |    |-> world_ctl_sservsup (supervisor)
      |    |    '-> world_ctl_sserv (gen_server)
      |    |-> world_sservsup (supervisor)
      |    |    '-> world_sserv (gen_server)
      |    '-> world_http
      '-> world_env (gen_server)
</pre>

#### world_sup.erl
The global supervisor which initialize the whole application. First the
one_for_one supervisor and then the simulated worlds.

#### world_ofo_sup.erl
A supervisor with one_for_one restart strategy which monitors all supervisor
ans worker which can be restarted without big trouble or dependencies.

#### world_env.erl
The gen_server which represents a virtual world and holds all the logic.

#### world_sservsup.erl and world_sserv.erl
The socket server which handle incoming connections and forward them to the
simulated world instance of world_env.erl  
Listens on the port which is defined in world.app as 'port' (default 4567)

#### world_ctl_sservsup.erl and world_ctl_sserv.erl
The socket server which handle incoming control connections and forward them
to the simulated world instance of world_env.erl. Also creates and destroys
worlds  
Listens on the port which is defined in world.app as 'ctl_port' (default 4568)

#### world_http.erl
A simple http server based on the build in httpd. Servers a static page with
a project description, a read only overview of the environments and some contact
informations. The environment overview is based on js and refreshs every second.

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
the command 'map' or 'world'. Encode the map with the ASCII symbols described
in the section Description; to mark the beginning of a new row use the
character |.
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
101 world changed (deprecated)  
102 environ [.|O|F|*]{8}  
103 free text help replies  
104 option listings  
105 ID, Name, X, Y, Agents, possible Agents  
105 ID X Y Agents MaxAgents  
106 world ID spawned
107 world ID loaded

200 Speak, friend, and ente(r)  
200 welcome in this [1-9]+x[1-9]+ world  
200 good bye  
201 [.|O|F|*]{8}  
202 food [0-9]+  
203 blocked  
204 staffed  
205 failed

300 bad argument  
301 death (not yet implemented)

400 unknown command  
403 access denied  
404 not found

500 sever made a boo boo  
501 world destroyed

#### Admin

world list  
world new [ASCII_REPRESENTATION]  
world load ID

map [ASCII_REPRESENTATION]  
options [OPTIONS ..]  
destroy  
kill all (not yet implemented)  
shutdown (not yet implemented)

quit

#### Client

world list  
world load ID  
move [0-8]  
environ  
help [COMMAND]

quit

## Used Sources

 * Erlang documentation
 * http://learnyousomeerlang.com/ for supervisor based socket server
 * http://www.uffmm.org/ for theoretical stuff and specifications
 * Twitter Bootstrap

## Licence

 * Source code: [GNU General Public License v3](http://www.gnu.org/licenses/gpl.html)
 * Documentation: [Creative Commons Attribution-ShareAlike 3.0 Unported License](http://creativecommons.org/licenses/by-sa/3.0/)
