#!/usr/bin/env python3
##
#: * @tests
#: ** `echo 'Héllò hôw are ü?' | unicode_normalizer_hf.py`
##
#: https://huggingface.co/docs/tokenizers/pipeline
from tokenizers import normalizers

#: * https://huggingface.co/docs/tokenizers/api/normalizers
#: ** NFD Unicode Normalizer
from tokenizers.normalizers import NFD, StripAccents


import sys

input = sys.stdin.read()

normalizer = normalizers.Sequence([NFD(), StripAccents()])

input_normalized = normalizer.normalize_str(input)

print(input_normalized)
