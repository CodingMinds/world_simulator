# target: all   	- Default target. Compiles .erl files
all:
	erlc *.erl

# target: clean 	- Remove compilation results
clean:
	rm 2> /dev/null *.beam

# target: help  	- Display callable targets
help:
	@egrep "^# target:" [Mm]akefile
