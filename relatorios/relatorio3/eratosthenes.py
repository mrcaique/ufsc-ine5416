#!/usr/bin/env python3
from sys import argv
from math import sqrt

# Sieve of Eratosthenes
# A simple algorithm that returns a list of
# primes to a given number

# Given a number, verify if it is a prime number
def sieve(number):
	limit = int(sqrt(number))
	for i in range(2, limit+1, 1):
		if (number % i == 0):
			return False
	return True

# main function
max_seq = int(argv[1])
print(2, end=' ')
for i in range(3, max_seq, 2):
	if (sieve(i)):
		print(i, end=' ')

print("\n")
