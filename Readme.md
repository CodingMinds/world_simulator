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
end of this Reamde file. The movement and position values are based on
the following table where 0 is the actual position if the agent (also
from Wilsons WOOD1):

8 | 1 | 2  
7 | 0 | 3  
6 | 5 | 4

Also compare with the corresponding script
http://www.uffmm.org/EoIESS-TH/gclt/node14.html  
Disclaimer: The author of the script is different from the authors of
the implementations.

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

 * Implement Fitness
 * Add more logging within world_env.erl for multiple worlds
 * Improve line ending (add CR)
 * Replace PIDs with unique names or sthg like this. list_to_pid is only
   for debugging purposes and erlang interna.

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
      '-> world_envsup (supervisor)
           '-> world_env (gen_server)
</pre>

#### world_sup.erl
The global supervisor which initialize the whole application. First the
one_for_one supervisor and then the simulated worlds.

#### world_ofo_sup.erl
A supervisor with one_for_one restart strategy which monitors all
supervisor ans worker which can be restarted without big trouble or
dependencies.

#### world_envsup.erl
The supervisor which manages all virtual environments/worlds.

#### world_env.erl
The gen_server which represents a virtual world and holds all the logic.

#### world_sservsup.erl and world_sserv.erl
The socket server which handle incoming connections and forward them to
the simulated world instance of world_env.erl  
Listens on the port which is defined in world.app as 'port' (default
4567)

#### world_ctl_sservsup.erl and world_ctl_sserv.erl
The socket server which handle incoming control connections and forward
them to the simulated world instance of world_env.erl. Also creates and
destroys worlds  
Listens on the port which is defined in world.app as 'ctl_port' (default
4568)

#### world_http.erl
A simple http server based on the build in httpd. Servers a static page
with a project description, a read only overview of the environments and
some contact informations. The environment overview is based on js and
refreshs every second.

#### world_logging.erl
A simple logging server which writes the messages from client and
environment changes/action into separate files.  
The location of the files is defined in world.app as log_client, log_env
and log_info.

#### world_helper.erl
Some helper functions which mostly used in more than one module.

## Default behaviour

The system is initialized with three example environments which show all
possible objects, expect other agents.

To load your own environment connect to the control port (see Example
usage) and use the commands 'world' and 'map' to load an existing
environment for your connection and replace the existing map (This will
drop all clients which are connected to this world). You can also spawn
a new environment with a new world if you use 'world spawn [ASCII]'.
Encode the map with the ASCII symbols described in the section
Description; to mark the beginning of a new row use the character |. For
example the defaultMap could be encoded with the following string:

<pre>
.....|.....|OO.|...OF|..X..|
</pre>

As you can see, the third row has only three elements. This should
demonstrate, that the system interprets each not explicit defined
section as a blocking object. And each unknown character (e.g. the X) is
interpreted as a free cell.

Also the environment has some configuration options which are not direct
tied to the map.

 * maximum amount of allowed agents (default: as much as possible)
 * should consumed food respwan (default: yes)
 * should consumed food respawn on the same place (default: yes)
 * the name of the environment (default: unknown)
 * possibility to define the initial start position of the agents
   (default: yes)
 * initial fitness (default: 1000)
 * fitness reduction if agent has not moved (default: 1)
 * fitness reduction if target section is blocked (default: 3)
 * fitness reduction if target section is staffed (default: 3)
 * fitness reduction if agent has moved (default: 2)

All this options can changed on runtime if you are connected with the
control port. The following command snippet show the active options and
then change the maximum client count to 7 and let the food respawn on
random places.

<pre>
options
104 max agents: 4
104 respawn food: true
104 static food positions: true
104 environment name: defaultMap
104 allow start position: true
104 initial fitness: 1000
104 fitness reduction if agent not moved: 1
104 fitness reduction if section blocked: 3
104 fitness reduction if section staffed: 3
104 fitness reduction if agent moved: 2
104 EOL
options 7 true false defaultMap true 1000 1 3 3 2
201 success
</pre>

Notice: There are more options planed. E.g. the amount of food if an
agent enters a food section, etc..

## Example Usage

#### Configuration

All ports and paths can be configured with ebin/world.app.

#### Compiling

Before the first run (or after sourcecode modifications) you need to
compile the .erl files. This can be done by hand
```sh
$ erlc --make
```
or with the include makefile
```sh
$ make
```

#### Running

To start the application start an erlang shell in the directory ebin and
enter
```erlang
1> application:start(world).
```

Then open a connection from another terminal and try to find the food
<pre>
$ telnet localhost 4567
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
200 Speak, friend, and ente(r)
world list
105 ID, Name, X, Y, Agents, possible Agents
105 <0.97.0> restrictiveDemo 9 9 0 2
105 <0.99.0> WOOD1 55 15 0 1
105 <0.95.0> defaultMap 5 5 0 4
105 EOL
world load <0.95.0>
200 welcome in this 5x5 world. Your ID is <0.56.0>
environ
102 environ OO...OOO
move 5
201 success ...OOOOO
move 3
201 success ....OO..
move 3
201 success ...O.O..
move 5
201 success ..OO..O.
move 5
201 success .OO....O
move 4
201 success OF.OOO..
move 2
202 food 1000
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
world list
105 ID, Name, X, Y, Agents, possible Agents
105 <0.97.0> restrictiveDemo 9 9 0 2
105 <0.99.0> WOOD1 55 15 0 1
105 <0.95.0> defaultMap 5 5 2 4
105 EOL
world load <0.95.0>
107 World <0.95.0> loaded
map
100  *1 *2 .  .  . 
100  .  .  .  .  . 
100  O  O  .  O  O 
100  .  .  .  O  F 
100  .  .  .  .  . 
100 EOL
map F...O|....|..........F
201 success
100  F  .  .  .  O  O  O  O  O  O  O 
100  .  .  .  .  O  O  O  O  O  O  O 
100  .  .  .  .  .  .  .  .  .  .  F 
100 EOL
quit
200 good bye
Connection closed by foreign host.
</pre>

## Demos

#### socket_demo.sci

A short Scilab socket connection demo.

#### user_interaction.sci

A short Scilab program which connects to the server and let user
interact with the world.

#### random_agent.py

A simple demo of a random acting agent which terminates if the food is
found.  
The amount of attempts will be printed to stdout.

#### guest_ui.php

A simple demo of a php UI to show interested users what's going on. Uses
the ctrl port to get the whole environment. (deprecated)

## Protocol DRAFT

#### Sever

100 ASCII map representation  
100 EOL  
101 world changed (deprecated)  
102 environ [.|O|F|*]{8}  
103 free text help replies  
104 option listings  
104 EOL  
105 ID, Name, X, Y, Agents, possible Agents  
105 ID X Y Agents MaxAgents  
105 EOL  
106 world <ID> spawned  
107 world <ID> loaded  
107 world <ID> destroyed

200 Speak, friend, and ente(r)  
200 welcome in this [1-9]+x[1-9]+ world. Your ID is <ID>  
200 good bye  
201 success [.|O|F|*]{8}  
202 food [0-9]+  
203 blocked  
204 staffed  
205 failed

300 bad argument  
301 death (not yet implemented)

400 unknown command  
403 access denied  
404 not found  
405 invalid position

500 sever made a boo boo  
501 world destroyed

#### Admin

world list  
world spawn [ASCII_REPRESENTATION]  
world load <ID>  
world destroy <ID>

map [ASCII_REPRESENTATION]  
options [OPTIONS ..]  
kill all (not yet implemented)  
shutdown (not yet implemented)

quit

#### Client

world list  
world load <ID> [X Y]  
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
