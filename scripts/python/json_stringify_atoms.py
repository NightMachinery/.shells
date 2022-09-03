#!/usr/bin/env python3

import sys
import json
from collections.abc import Iterable

input_json = sys.stdin.read()

input_dict = json.loads(input_json)

def stringify_atoms(some_container):
    if some_container is None or isinstance(some_container, str):
        return some_container
    elif isinstance(some_container, dict):
        for key in some_container.keys():
            some_container[key] = stringify_atoms(some_container[key]) #: @mutates the input

        return some_container
    elif isinstance(some_container, Iterable):
        return list(map(stringify_atoms, some_container))
    else:
        return str(some_container) #: actually an atom, not a container

print(json.dumps(stringify_atoms(input_dict)))
