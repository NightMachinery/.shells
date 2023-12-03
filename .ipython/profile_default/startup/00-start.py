##
# import torch.nn.functional as F
try:
    import jax.nn as jnn
except:
    pass
##
from rich import inspect


def h(x, **kwargs):
    return inspect(x, help=True, **kwargs)


##
from datetime import datetime, timedelta
import pathlib
from pathlib import Path
import os
import sys
import re
import random
import math
import logging

logger = logging.getLogger("ipy")

try:
    import numpy

    np = numpy
except:
    pass

try:
    import pandas

    pd = pandas
except:
    pass

try:
    from brish import z, zp, zs, zq, bsh
except:
    pass
try:
    # from icecream import ic
    from pynight.common_icecream import ic
except ImportError:  # Graceful fallback if IceCream isn't installed.
    ic = lambda *a: None if not a else (a[0] if len(a) == 1 else a)  # noqa

###
from functools import wraps
from time import time


def timing(f):
    @wraps(f)
    def wrap(*args, **kw):
        ts = time()
        result = f(*args, **kw)
        te = time()
        print("func:%r args:[%r, %r] took: %2.4f sec" % (f.__name__, args, kw, te - ts))
        return result

    return wrap


###
def char_range(c1, c2):
    """Generates the characters from `c1` to `c2`, inclusive."""
    for c in range(ord(c1), ord(c2) + 1):
        yield chr(c)


### * LLM
from pynight.common_bells import bell_gpt
from pynight.common_openai import (
    openai_key_get,
    setup_openai_key,
    print_chat_streaming,
)
from pynight.common_openai import *
from pynight.common_llm import (
    chat,
    conversations,
    # llm_models,
)
from pynight.common_spacy import spacy_sentencizer, spacy_sentencizer_fa


setup_openai_key()
###
