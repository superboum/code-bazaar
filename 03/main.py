#!/bin/python3
import functools

def one_more_prime(primes):
    tested_number = primes[-1]+1
    while True:
        can_be_a_prime = True
        for prime in primes:
            if tested_number % prime == 0:
                tested_number += 1
                can_be_a_prime = False
                break

        if can_be_a_prime:
            primes.append(tested_number)
            break

    return primes


target = int(input())
primes = [2]
factors = [1]

if target % primes[-1] == 0:
    factors.append(primes[-1])

while functools.reduce(lambda x, y: x*y, factors) != target:
    primes = one_more_prime(primes)
    if target % primes[-1] == 0:
        factors.append(primes[-1])
print("This solution does not work for every value (eg: 8)")
print(factors)
