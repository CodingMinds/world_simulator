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
	
	while "EOL" not in data:
		s.sendall("world list\r\n")
		data = s.recv(1024) # read world list
		match = re.findall(' <(.+?)> ', data)
		if world:
			match.insert(0, world)
		if not match:
			break
		for w in match:
			s.sendall("world load <" + w + ">\r\n")
			sdata = s.recv(1024)
			if "200" in sdata:
				found = 1
				print >> sys.stderr, "Use world <" + w + ">."
				break
	if not found:
		print >> sys.stderr, "No world available."
		sys.exit()
except socket.error, msg:
	print >> sys.stderr, "Failed to communicate with server."
	sys.exit()

# go and search food
try:
	data = ""
	counter = 0
	
	s.sendall("environ\r\n")
	data = s.recv(1024) #read environ
	
	while 1:
		counter+=1
		
		move = random.randint(1,8)
		env = re.findall('[\.FO*]', data)
		if "F" in env:
			move = env.index("F") + 1
		while env[move-1] == "O" or env[move-1] == "*":
			move = random.randint(1,8)
		
		s.sendall("move " + str(move) + "\r\n")
		data = s.recv(1024)
		
		if verbose:
			print representation[move]
			if "food" in data:
				print data
		if not infinite and "food" in data:
			break
		while not "201" in data and not "102" in data:
			s.sendall("environ\r\n")
			data = s.recv(1024)
		if sleep:
			time.sleep(sleep)
	print counter
except socket.error, msg:
	print >> sys.stderr, "Failed to communicate with server."
	sys.exit()

# close connection
s.close()
