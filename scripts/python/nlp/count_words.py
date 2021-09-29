#!/usr/bin/env python3

import sys
import os
from icecream import ic
import spacy
import io

def main(text_loc):
    with io.open(text_loc, 'r', encoding='utf8') as file_:
       text = file_.read()

    isDbg = os.environ.get("DEBUGME", False)

    # =python -m spacy download en_core_web_sm=
    # https://spacy.io/usage/models
    #
    # model_name = "en_core_web_lg"
    model_name = "en_core_web_sm" # much faster: 1min vs 5min
    nlp = spacy.load(model_name)
    print(f"Loaded model {model_name}", file=sys.stderr)

    doc = nlp(text)
    word_count_reject = 0
    word_count_accept = 0
    for token in doc:
        if isDbg:
            print(token.text, token.lemma_, token.pos_, token.is_stop, file=sys.stderr)

        pos_accept_list = ['NOUN', 'PROPN', 'ADJ', 'VERB', 'INTJ']
        pos_reject_list = ['SPACE', 'PUNCT', 'SYM', 'X', 'NUM']

        part_of_speech = token.pos_
        if part_of_speech in pos_accept_list:
            word_count_accept += 1
        if part_of_speech not in pos_reject_list:
            word_count_reject += 1

    ic(word_count_accept)
    ic(word_count_reject)

    print(word_count_reject)

if __name__ == '__main__':
    main(sys.argv[1])
