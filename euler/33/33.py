#!/usr/bin/python3
from fractions import Fraction
failed = Fraction(1,1)

def compare_frac(num1, dem1, num2, dem2):
    if dem1 == 0 or dem2 == 0: return failed
    if num2 == dem2: return failed
    f1 = Fraction(num1, dem1)
    f2 = Fraction(num2, dem2)
    if f1 >= 1: return failed
    if f1 == f2: 
        print(f"{num1}/{dem1}, {num2}/{dem2}, {f1}")
        return f1
    return failed

acc = failed
for a in range(1,10):
    for b in range(10):
        for c in range(1,10):
            for d in range(10):
                ab = int(f"{a}{b}")
                cd = int(f"{c}{d}")
                if b == 0 and d == 0: continue
                if a == c: acc *= compare_frac(b, d, ab, cd)
                if a == d: acc *= compare_frac(b, c, ab, cd)
                if b == c: acc *= compare_frac(a, d, ab, cd)
                if b == d: acc *= compare_frac(a, c, ab, cd)

print(acc)
