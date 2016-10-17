#!/usr/bin/python3

digit_number = int(input())
init=int("9"*digit_number)
delta = 1
palindromes = []

while len(palindromes) < 1:
    for i in range(int(delta/2)+1):
        n1 = init - i
        n2 = init - delta + 1 + i
        if str(n1*n2) == str(n1*n2)[::-1]:
            palindromes.append(n1*n2)
    delta += 1

print(sorted(palindromes))
