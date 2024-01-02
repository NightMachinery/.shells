#!/usr/bin/env python3
##
import os
import sys
from datetime import date, timedelta
from pynight.common_icecream import ic

from pynight.common_openai import (
    openai_key_get,
    setup_openai_key,
    print_chat_streaming,
)

from llama_cpp import Llama
from llama_cpp import ChatCompletionMessage


HOME = os.environ["HOME"]

##
model_path = f"{HOME}/base/models/LLM/Deepseek/deepseek-coder-6.7b-instruct.Q8_0.gguf"

stream_p = False

query = ' '.join(sys.argv[1:])
# ic(query)

user_prompt=f"What date is {query}?"
# user_prompt="What date is the last Tue?"
##

llm = Llama(
    model_path=model_path,
    # model_path=f"{HOME}/base/models/LLM/Starling/starling-lm-7b-alpha.Q8_0.gguf",
    n_ctx=2048,
    # n_ctx=2048,
    # n_ctx=512,
    n_gpu_layers=-1,
    # chat_format="llama-2",
    # chat_format="openchat",
)

##
# output = llm.create_chat_completion(
#     messages=[
#         {"role": "system", "content": "You are a senior programming assistant. You only output the requested code in a code block."},
#         {
#             "role": "user",
#             "content": """What date is the second last Thursday 13:45PM? Give a Python script that outputs the answer.""",
#         },
#         {
#             "role": "assistant",
#             "content": """```python\n""",
#         },
#     ],
#     max_tokens=512,
#     # stop=["\n"],
#     temperature=0,
#     stream=True,
# )
##
#: 0. Monday
#: 1. Tuesday
#: 2. Wednesday
#: 3. Thursday
#: 4. Friday
#: 5. Saturday
#: 6. Sunday
##
user_prompt1 = f"""What date is the second last Thursday 13:45PM?"""

assistant_prompt1 = r"""```python
from datetime import datetime, timedelta

# Function to find the second last Thursday at 13:45 PM
def find_second_last_thursday():
    # Get the current date and time
    now = datetime.now()

    # No need to check if today is Thursday or not, since the query asks for a specific time of the date.
    # Find the last Thursday
    last_thursday = now - timedelta(days=(now.weekday() - 3) % 7)

    # Set the time to 13:45
    last_thursday = last_thursday.replace(hour=13, minute=45, second=0, microsecond=0)

    # Find the second last Thursday by subtracting 7 days
    second_last_thursday = last_thursday - timedelta(weeks=1)

    return second_last_thursday

# Call the function and print the result
print(f"{find_second_last_thursday()}")
```
"""
#    # If it's currently past 13:45 on a Thursday, we start counting from the previous week
#    if now.weekday() == 3 and now.hour > 13 or (now.hour == 13 and now.minute > 45):
#        now -= timedelta(weeks=1)

user_prompt2="What date is the next Wed?"
assistant_prompt2 = """```python
from datetime import datetime, timedelta

# Function to find the next Wednesday
def find_next_wednesday():
    # Get the current date
    today = datetime.now()

    # Calculate how many days to add to get to the next Wednesday
    # Wednesday is represented by 2 in Python's datetime module (Monday is 0)
    days_until_wednesday = (2 - today.weekday()) % 7
    if days_until_wednesday == 0:
        # If today is Wednesday, we want the next Wednesday, not today
        days_until_wednesday = 7

    # Add the necessary number of days to get the next Wednesday
    next_wednesday = today + timedelta(days=days_until_wednesday)

    # Return the date without the time component
    return next_wednesday.date()

# Call the function and print the result
print(f"{(find_next_wednesday())}")
```
"""

assistant_prompt = "\n```python\n"
# assistant_prompt = ""


complete_prompt = f"""You are an AI programming assistant, utilizing the Deepseek Coder model, developed by Deepseek Company. You write Python scripts that answer the questions.
### Instruction:
{user_prompt1}
### Response:
{assistant_prompt1}
### Instruction:
{user_prompt2}
### Response:
{assistant_prompt2}
### Instruction:
{user_prompt}
### Response:
{assistant_prompt}
"""

# complete_prompt = f"GPT4 User: {user_prompt}<|end_of_turn|>GPT4 Assistant: {assistant_prompt}"

output = llm(
    complete_prompt,
    max_tokens=2048,
    # stop=["\n"],
    # stop=["</s>"],
    stop=["\n```"],
    echo=False,
    # echo=True,
    # logprobs=True,
    temperature=0,
    # top_p=1,
    min_p=0.05,
    stream=stream_p,
)

if stream_p:
    print_chat_streaming(
        output,
        # debug_p=True,
    )
else:
    # ic(output)
    print(output['choices'][0]['text'])
