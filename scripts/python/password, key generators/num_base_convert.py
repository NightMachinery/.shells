#!/usr/bin/env python3
import sys

input = sys.argv[1]
from_base = int(sys.argv[2])
to_base = int(sys.argv[3])

def decimal_to_base(n, b):
    if n == 0:
        return [0]
    digits = []
    while n:
        new_digit = int(n % b)
        if new_digit >= 10:
            #: 65 is the ASCII code for 'A'.
            new_digit = chr(65 + (new_digit - 10))

        digits.append(new_digit)
        n //= b
    return digits[::-1]

print("".join(map(str, decimal_to_base(int(input, from_base), to_base))))
