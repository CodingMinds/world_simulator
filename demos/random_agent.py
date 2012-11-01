#!/usr/bin/env python

# A simple demo of a random acting agent which terminates if he found food
# The amount of attempts will be printed to stdout
# 01.11.12 M. Bittorf <info@coding-minds.com>

import random
import socket
import sys

# config
host = 'localhost'
port = 4567

# create an INET, STREAMing socket
try:
	s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
except socket.error, msg:
	print "Failed to create socket. Error code: " + str(msg[0])
	+ " , Error message : " + msg[1]
	sys.exit()

# resolve
try:
	remote_ip = socket.gethostbyname( host )
except socket.gaierror:
	print 'Hostname could not be resolved. Exiting'
	sys.exit()

# connect and receive greeting
s.connect((host, port))
s.recv(1024)

# go and search food
try:
	data = ""
	counter = 0
	while "food" not in data:
		counter+=1
		move = random.randint(1,8)
		s.sendall("move " + str(move) + "\r\n")
		data = s.recv(1024)
#	print "Found food after " + str(counter) + " attempts."
	print counter
except socket.error, msg:
	print "Failed to communicate with server. Error code: " + str(msg[0])
	+ " , Error message : " + msg[1]
	sys.exit()

# close connection
s.close()
