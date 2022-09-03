#!/usr/bin/env python3

from transformers import pipeline
import sys

input_text = sys.stdin.read() # .encode("utf8")

summarizer = pipeline("summarization")

summary_result = summarizer(input_text)

print(summary_result['summary_text'])
