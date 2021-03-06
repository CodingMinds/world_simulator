#!/usr/bin/env python

# A short dynamic learning classifier system where the function which
# applies the fitness values and the stored memory to the classifier
# list can be simple switched. Also some options like world,
# startposition and the size of the memory can be configured. To
# increase the detailed output play with the verbose variable, 3 would
# be a good value to test your implemented algorithm.
# The amount of attempts per connection will be printed to stdout.
# 21.11.12 M. Bittorf <info@coding-minds.com>

import random
import socket
import sys
import re
import time
from operator import itemgetter, attrgetter
from itertools import groupby

## config
host = 'localhost'
port = 4567

world = '' # without <>
startposition = '' # 'x y'

memory_size = 10
max_quality = 0 # 0 = disabled
ignore_repetitions = False
initial_classifiers = list() # could be a list of tuples
# like: ("OOFOOFOO", 3, 12)

iterations = 100 # -1 = infinite
ignore_failed_iterations = True

verbose = 1 # 0 - 5 / 10
sleep = 0 # seconds
reconnect_after_fitness = True # experimental ! (verbose: min 3)

### Essential algorithm
### Implement here how you would handle the fitness !
### 
### Available data:
### classifier_list (global): Holds all classifiers as tuples of
###  (condition, action, quality), where condition is the list with the
###  splited environ string, action the move direction as string and
###  quality an integer which represents the priority of this classifier.
### fitness: The fitness as integer
### memory: A list like classifier_list with the latest applied
###  classifiers (compare config variable memory_size). The first
###   element is the last used one, etc.
def apply_fitness_algorithm(fitness, memory):
	real_memory_size = len(memory)
	# apply the fitness value
	# based on the algorithm from http://www.uffmm.org/EoIESS-TH/gclt/node20.html
	p = fitness / sum(range(1, real_memory_size))
	
	for i in range(0, real_memory_size):
		try:
			classifier_list.remove(memory[i])
		except ValueError:
			pass
		
		(condition, action, quality) = memory[i]
		quality += (memory_size - i) * p
		
		classifier_list.append((condition, action, quality))

####### -- don't touch if you don't known what are you doing -- #######

## static stuff
representation = [".", "N", "NO", "O", "SO", "S", "SW", "W", "NW"]
classifier_list = initial_classifiers
last_classifiers = list() # of tuples

## function definitions

# applies the memory to the classifier_list. prepares the data and calls
# the user implementation to apply the fitness.
def apply_fitness(fitness):
	global classifier_list
	# extract the last used classifiers from memory
	classifiers = last_classifiers[-memory_size::1]
	classifiers.reverse()
	
	# is there a memory ?
	if len(classifiers) <= 0:
		return
	
	# call user function
	apply_fitness_algorithm(fitness, classifiers)
	
	# merge duplicate condition/action pairs
	# thx to Gribouillis (http://www.daniweb.com/software-development/python/threads/375081/merge-lists-when-certain-list-items-match#post1614203)
	def pair(item):
		return tuple(item[:2])
	
	classifier_list = [ (c, a, sum(q[2] for q in g)) for (c, a), g in groupby(sorted(classifier_list, key=pair), key=pair) ]
	
	# apply max_quality option
	if max_quality:
		for (condition, action, quality) in classifier_list:
			if quality > max_quality:
				# are you debugging smthg ?
				if 10 <= verbose:
					print "max_quality reached"
					print (condition, action, quality)
				classifier_list.remove((condition, action, quality))
				classifier_list.append((condition, action, max_quality))
	
	# sort it
	classifier_list.sort(key=itemgetter(2), reverse=True)
	
	# are you curious ?
	if 3 <= verbose:
		for condition, action, quality in classifier_list:
			print condition, "\t", action, "\t", quality

# creates an random classifier
def create_classifier(environ):
	# cast of dice
	action = random.randint(1,8)
	
	return environ, action, 0

# returns a matching classifier or creates a new one
def select_classifier(environ, classifier_list):
	for (condition, action, quality) in classifier_list:
		if condition == environ:
			return (condition, action, quality)
	return create_classifier(environ)

## enter the challenge
while -1 == iterations or iterations > 0:
	iterations-=1
	
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
		found = 0
		
		# request world list
		s.sendall("world list\r\n")
		if 10 <= verbose:
			print "world list"
		while "EOL" not in data: # until end of list
			data = s.recv(1024) # read world list
			
			# are you debugging smthg ?
			if 10 <= verbose:
				print data
			
			# extract all worlds
			match = re.findall(' <(.+?)> ', data)
			
			# prepend configured world if set
			if world:
				match.insert(0, world)
			
			# is there a world ?
			if not match:
				break
			
			# try to load one of the worlds
			for w in match:
				if 2 <= verbose:
					print "try world <" + w + "> " + startposition
				if startposition:
					s.sendall("world load <" + w + "> " + startposition + "\r\n")
					# are you debugging smthg ?
					if 10 <= verbose:
						print "world load <" + w + "> " + startposition
				else:
					s.sendall("world load <" + w + ">\r\n")
					# are you debugging smthg ?
					if 10 <= verbose:
						print "world load <" + w + ">"
				
				sdata = s.recv(1024)
				
				# are you debugging smthg ?
				if 10 <= verbose:
					print sdata
				
				if "200" in sdata: # great. world is available
					found = 1
					print >> sys.stderr, "Use world <" + w + ">."
					break
		
		# no world available oder usable
		if not found:
			print >> sys.stderr, "No world available."
			s.close()
			sys.exit()
	# smthg bad hapend. fix it !
	except socket.error, msg:
		print >> sys.stderr, "Failed to communicate with server while loading world."
		print >> sys.stderr, "Still", iterations, "iterations pending."
		print >> sys.stderr, "Cleanup and start next try."
		classifier_list = initial_classifiers
		last_classifiers = list() # of tuples
		if ignore_failed_iterations:
			iterations+=1
		continue
	
	# go and search food
	try:
		data = ""
		counter = 0
		
		# get initial environ
		s.sendall("environ\r\n")
		data = s.recv(1024)
		
		# are you debugging smthg ?
		if 10 <= verbose:
			print "environ"
			print data
		
		while 1:
			# cleanup environ and separate fitness and environ
			clean_data = ''.join(re.findall('[0-9]+:[\.FO*]+', data))
			fitness, sep, env = clean_data.partition(":")
			
			# are you curious ?
			if 4 <= verbose:
				print fitness, env
			
			# task done ?
			if int(fitness) != 0:
				apply_fitness(int(fitness))
				if reconnect_after_fitness:
					break
			
			# choose classifier
			condition, action, quality = select_classifier(env, classifier_list)
			
			# do it !
			s.sendall("move " + str(action) + "\r\n")
			data = s.recv(1024)
			counter+=1
			
			# are you debugging smthg ?
			if 10 <= verbose:
				print "move " + str(action)
				print data
			
			# remember, remember, ..
			if ignore_repetitions and len(last_classifiers) > 0:
				(last_c, last_a, last_q) = last_classifiers[-1]
				if last_c != condition and last_a != action:
					last_classifiers.append((condition, action, quality))
			else:
				last_classifiers.append((condition, action, quality))
			
			# are you curious ?
			if 5 <= verbose:
				print representation[action]
			
			# get new environ string if missing
			while not ":" in data:
				s.sendall("environ\r\n")
				data = s.recv(1024)
				
				# are you debugging smthg ?
				if 10 <= verbose:
					print "environ"
					print data
			
			# should we pause a while ?
			if sleep:
				time.sleep(sleep)
		
		print counter
	# smthg bad happend. fix it !
	except socket.error, msg:
		print >> sys.stderr, "Failed to communicate with server while searching food."
		print >> sys.stderr, "Still", iterations, "iterations pending, this was guess", counter
		print >> sys.stderr, "Cleanup and start next try."
		classifier_list = initial_classifiers
		last_classifiers = list() # of tuples
		if ignore_failed_iterations:
			iterations+=1
		continue
	
# close connection
	s.close()

# wanna see the results ?
if verbose:
	for condition, action, quality in classifier_list:
		print condition, "\t", action, "\t", quality