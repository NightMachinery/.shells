#!/usr/bin/env python
import wikiquotes
from random import randint
quote_source = randint(0,5)
if quote_source==0:
    quote_page="Alice's Adventures in Wonderland"
elif quote_source == 1:
    quote_page = "American McGee's Alice"
elif quote_source == 2:
    quote_page = "Through the Looking-Glass"
elif quote_source == 3:
    quote_page = "Into the Woods"
elif quote_source == 4:
    quote_page = "Alice in Wonderland (2010 film)"
else:
    quote_page='Alice: Madness Returns'
print(wikiquotes.random_quote(quote_page,'english'))
