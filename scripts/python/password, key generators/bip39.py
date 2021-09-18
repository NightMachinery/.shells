#!/usr/bin/env python3

from mnemonic import Mnemonic
import sys, os

passphrase = os.environ.get("bip39_py_p", "")
if passphrase == '-':
    print("Reading the passphrase from stdin ...\n", file=sys.stderr)

    passphrase = sys.stdin.read()

if passphrase == '':
    print("@warn using an empty passphrase ...\n", file=sys.stderr)

mnemo = Mnemonic("english")

words = mnemo.generate(strength=256)
seed = mnemo.to_seed(words, passphrase=passphrase)
entropy = mnemo.to_entropy(words)

outputs=['words', 'seed', 'entropy']
if len(sys.argv) > 1:
    outputs=sys.argv[1:]

for o in outputs:
    print(f"{o}:\n\t{globals()[o]}\n")
