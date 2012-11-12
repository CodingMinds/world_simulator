# target: erlang	- Compiles .erl files
erlang:
	mkdir -p log
	erl -make	

# target: clean 	- Remove compilation results
clean:
	rm 2> /dev/null ebin/*.beam

# target: edoc		- Buidl edoc with rebar
edoc:
	rebar doc

# target: help  	- Display callable targets
help:
	@egrep "^# target:" [Mm]akefile

# target: all   	- Default target. Calls clean and erlang
all:
	clean
	erlang

