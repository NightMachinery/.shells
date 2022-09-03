#!/usr/bin/env python3
#: * See the latest version at the fanfic-classifier notebook.
#: ** quite heavyweight, not runnable on my MBP after 25 minutes with the input 'hello'
#: * quality not acceptable on novels
##

import torch
import sys
from transformers import LEDTokenizer, LEDForConditionalGeneration

model = LEDForConditionalGeneration.from_pretrained("allenai/led-large-16384-arxiv")
tokenizer = LEDTokenizer.from_pretrained("allenai/led-large-16384-arxiv")

ARTICLE_TO_SUMMARIZE = sys.stdin.read()

inputs = tokenizer.encode(ARTICLE_TO_SUMMARIZE, return_tensors="pt")

# Global attention on the first token (cf. Beltagy et al. 2020)
global_attention_mask = torch.zeros_like(inputs)
global_attention_mask[:, 0] = 1

# Generate Summary
summary_ids = model.generate(inputs, global_attention_mask=global_attention_mask, num_beams=10, max_length=1024)
print(tokenizer.decode(summary_ids[0], skip_special_tokens=True, clean_up_tokenization_spaces=True))
