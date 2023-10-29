#!/usr/bin/env python3

import sys
import json5
from collections.abc import Iterable


def generate_imenu_index(json_object, prefix=None):
    """Generate imenu index for a given JSON object."""
    result = []

    if isinstance(json_object, dict):
        for key, value in json_object.items():
            full_key = key if prefix is None else f"{prefix}.{key}"
            if isinstance(value, (dict, list)):
                result.append((full_key, generate_imenu_index(value, full_key)))
            else:
                result.append((full_key, None))
    elif isinstance(json_object, list):
        for i, value in enumerate(json_object):
            full_key = str(i) if prefix is None else f"{prefix}[{i}]"
            if isinstance(value, (dict, list)):
                result.append((full_key, generate_imenu_index(value, full_key)))
            else:
                result.append((full_key, None))
    return result


if __name__ == "__main__":
    data = json5.loads(sys.stdin.read())
    index = generate_imenu_index(data)
    print(index)
