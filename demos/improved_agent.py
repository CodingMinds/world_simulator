#!/usr/bin/env python

# A simple demo of a random acting agent which terminates if he found
# food. The agent loads the configured or first possible world and
# interprets food and objects to improve his behaviour.
# The amount of attempts will be printed to stdout.
# 01.11.12 M. Bittorf <info@coding-minds.com>
# 16.11.12 M. Bittorf <info@coding-minds.com> (updated)

import random
import socket
import sys
import re
import time

# config
host = 'localhost'
port = 4567
world = ''
verbose = 0 # true / false
infinite = 0 # true / false
sleep = 0 # seconds

# static stuff
representation = [".", "N", "NO", "O", "SO", "S", "SW", "W", "NW"]
reverse = [-1, 5, 6, 7, 8, 1, 2, 3, 4]

# create an INET, STREAMing socket
try:
	s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
except socket.error, msg:
	print >> sys.stderr, "Failed to create socket. Error code: "
	+ str(msg[0]) + " , Error message : " + msg[1]
	sys.exit()

# resolve
try:
	remote_ip = socket.gethostbyname( host )
except socket.gaierror:
	print >> sys.stderr, 'Hostname could not be resolved. Exiting'
	sys.exit()

# connect and receive greeting
s.connect((host, port))
s.recv(1024)

# try to load a world
try:
	data = ""
	skip = 0
	found = 0
	
	# request world list
	s.sendall("world list\r\n")
	while "EOL" not in data: # until end of list
		data = s.recv(1024) # read world list
		match = re.findall(' <(.+?)> ', data) # extract all worlds
		
		# prepend configured world if set
		if world:
			match.insert(0, world)
		
		# is there a world ?
		if not match:
			break
		
		# try to load one of the worlds
		for w in match:
			s.sendall("world load <" + w + ">\r\n")
			sdata = s.recv(1024)
			if "200" in sdata: # great world is available
				found = 1
				print >> sys.stderr, "Use world <" + w + ">."
				break
	
	# no world available oder usable
	if not found:
		print >> sys.stderr, "No world available."
		sys.exit()
# smthg bad hapend
except socket.error, msg:
	print >> sys.stderr, "Failed to communicate with server."
	sys.exit()

# go and search food
try:
	data = ""
	counter = 0
	last_move = 0
	
	# get initial environ
	s.sendall("environ\r\n")
	data = s.recv(1024)
	
	while 1:
		counter+=1
		
		# cast of dice
		move = random.randint(1,8)
		
		 # cleanup environ - we don't need the fitness value
		env = re.findall('[\.FO*]', data)
		
		# catch the food if it's visible
		if "F" in env:
			move = env.index("F") + 1
		
		 # cast again if there are something block the path
		while env[move-1] == "O" or env[move-1] == "*":
			move = random.randint(1,8)
		
		# disallow to go back to previous position if there are other paths
		senv = re.findall('[\.F]', data)
		while len(senv) > 1 and reverse[last_move] == move:
			move = random.randint(1,8)
		
		# do it !
		s.sendall("move " + str(move) + "\r\n")
		data = s.recv(1024)
		last_move = move
		
		# a little bit verbosity
		if verbose:
			print representation[move]
			if "food" in data:
				print data
		
		# die after food consumption ?
		if not infinite and "food" in data:
			break
		
		# get new environ string if missing
		while not "201" in data and not "102" in data: #
			s.sendall("environ\r\n")
			data = s.recv(1024)
		
		# should we pause a while ?
		if sleep:
			time.sleep(sleep)
	print counter
# smthg bad hapend
except socket.error, msg:
	print >> sys.stderr, "Failed to communicate with server."
	sys.exit()

# close connection
s.close()
