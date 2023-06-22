#!/usr/bin/env python3
##
import sys
import os
from bs4 import BeautifulSoup


def clean_html(input_html):
    soup = BeautifulSoup(input_html, "html.parser")

    for tag in soup():
        if tag.name == "meta":
            #: Remove meta tags
            tag.decompose()
        else:
            #: Remove unwanted attributes
            ##
            # for attribute in ["class", "style", "src", "xmlns"]:
            #     del tag[attribute]
            ##
            attrs_whitelist = ["href", "title", "alttext"]
            attrs_to_delete = [attr for attr in tag.attrs if attr not in attrs_whitelist]
            for attr in attrs_to_delete:
                del tag[attr]

    return str(soup)


html_data = sys.stdin.read()
cleaned_html = clean_html(html_data)
print(cleaned_html)
