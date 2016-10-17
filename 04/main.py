#!/usr/bin/python3

digit_number = int(input())

nombres=[int("9"*digit_number)]

palindromes=[]

while len(palindromes) < 1:
    for i in nombres:
        for j in nombres:
            if i < j: pass
            elif str(i*j) == str(i*j)[::-1]:
                palindromes.append(i*j)
    nombres.append(nombres[-1]-1)

print(sorted(palindromes))
