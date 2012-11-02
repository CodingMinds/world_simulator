# target: all   	- Default target. Compiles .erl files
all:
	erl -make

# target: clean 	- Remove compilation results
clean:
	rm 2> /dev/null ebin/*.beam

# target: help  	- Display callable targets
help:
	@egrep "^# target:" [Mm]akefile
