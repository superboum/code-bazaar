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

def get_decomposition(val, primes):
    decompo = []
    for i in primes:
        if val % i == 0:
            decompo.append(i)
            val = int(val / i)
            if val == 1: return decompo
            decompo += get_decomposition(val, primes)
            return decompo
    return decompo

N = int(input())
decompositions = {}
primes = [2]
while primes[-1] < N: primes = one_more_prime(primes)

#print(primes)
#print(get_decomposition(N, primes))
for i in range(2,N+1):
    res = get_decomposition(i, primes)
    res_count = dict((i, res.count(i)) for i in res)
    for key, value in res_count.items():
        if key in decompositions:
            decompositions[key] = max(decompositions[key], value)
        else:
            decompositions[key] = value

res = 1
for k, v in decompositions.items():
    res *= pow(k,v)

print(res)
