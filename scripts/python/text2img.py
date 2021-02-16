#!/usr/bin/env python3

##
# install: pip install --upgrade arabic-reshaper python-bidi Pillow
# usage: echo <text> | text2img.py <font> <output>
##

import arabic_reshaper
import sys

from bidi.algorithm import get_display

from PIL import ImageFont
from PIL import Image
from PIL import ImageDraw

# use a good font!
fontFile = sys.argv[1]

imageOut = sys.argv[2]

# load the font and image
font = ImageFont.truetype(fontFile, 21)
image = Image.new("RGB", (1200, 600), (255, 255, 255))

# first you must prepare your text (you dont need this step for english text)
text = sys.stdin.read()
reshaped_text = arabic_reshaper.reshape(text)    # correct its shape
bidi_text = get_display(reshaped_text)           # correct its direction

# start drawing on image
draw = ImageDraw.Draw(image)
draw.text((0, 0), bidi_text, (0, 0, 0), font=font)
draw = ImageDraw.Draw(image)

# save it
image.save(imageOut, "PNG")
