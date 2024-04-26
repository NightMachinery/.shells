#: Errors here will be silently ignored by emacs-jupyter, but the rest of the code won't run.
##
try:
    import torch
    import torch.nn.functional as F
except:
    pass

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
    openai_chat_complete,
    openai_text_complete,
    openai_image_url_auto,
)
from pynight.common_openai import *
from pynight.common_groq import (
    setup_groq_key,
    groq_key_get,
)
from pynight.common_deepseek import (
    setup_deepseek_key,
    deepseek_key_get,
)
from pynight.common_together import (
    setup_together_key,
    together_key_get,
)

from pynight.common_llm import (
    chat,
    conversations,
    # llm_models,
)
from pynight.common_spacy import spacy_sentencizer, spacy_sentencizer_fa


openai_client = setup_openai_key()
openrouter_client = setup_openrouter_key()
groq_client = setup_groq_key()
deepseek_client = setup_deepseek_key()
together_client = setup_together_key()
###
from pynight.common_anthropic import (
    setup_anthropic_key,
    anthropic_key_get,
)

anthropic_client = setup_anthropic_key()
###
