#!/usr/bin/env python3

##
# install: pip install --upgrade arabic-reshaper python-bidi Pillow numpy blend_modes
# usage: echo <text> | text2img.py <font> <output>
##

import arabic_reshaper
import sys
import os

from bidi.algorithm import get_display

from PIL import ImageFont
from PIL import Image
from PIL import ImageDraw
import numpy
import blend_modes # @alt https://github.com/akiomik/pilgram

try:
    from IPython import embed
except:
    pass

text = sys.stdin.read()
try:
    sys.stdin = open('/dev/tty')
except:
    pass

fontFile = sys.argv[1]

imageOut = sys.argv[2]
imageIn = None
if len(sys.argv) >= 4:
    imageIn = sys.argv[3]
###
s = int(os.environ.get("text2img_s", 33))
font = ImageFont.truetype(fontFile, s, encoding='unic', layout_engine=ImageFont.LAYOUT_RAQM)
##
# `encoding=...` might not be necessary (idk what it does)
## color emoji:
# '/System/Library/Fonts/Apple Color Emoji.ttc'
# 'https://fontlibrary.org/en/font/symbola' black and white texty emojis, does not support Persian
# Testing, I found that the following sizes work with Emojis - 20, 32, 40, 48, 64, 96, 160
# https://github.com/python-pillow/Pillow/issues/1422
## use Symbola for normal emoji
###
image = None
imgA = None
if imageIn:
    image = Image.open(imageIn)
    imgA = numpy.array(image)
    # append alpha channel
    # embed()
    imgA = numpy.dstack((imgA, numpy.ones((imgA.shape[0], imgA.shape[1], 1))*255))
    imgA = imgA.astype(float)
    image = Image.new("RGBA", (image.size[0], image.size[1]), (0, 0, 255, 0))
else:
    w = int(os.environ.get("text2img_w", 1400))
    h = int(os.environ.get("text2img_h", 600))
    bgr = int(os.environ.get("text2img_bgr", 255))
    bgg = int(os.environ.get("text2img_bgg", 255))
    bgb = int(os.environ.get("text2img_bgb", 255))
    image = Image.new("RGB", (w, h), (bgr, bgg, bgb))

# first you must prepare your text (you dont need this step for english text)
reshaped_text = arabic_reshaper.reshape(text)    # correct its shape
bidi_text = get_display(reshaped_text)           # correct its direction

# start drawing on image
draw = ImageDraw.Draw(image)

x = int(os.environ.get("text2img_x", 0))
y = int(os.environ.get("text2img_y", 0))

bold = bool(os.environ.get("text2img_bold", False))

r = int(os.environ.get("text2img_r", 0))
g = int(os.environ.get("text2img_g", 0))
b = int(os.environ.get("text2img_b", 0))

if bold:
    # r2, g2, b2 = (0, 0, 200)
    # r2, g2, b2 = (50, 50, 50)
    # r2, g2, b2 = (255, 255, 255)
    # r2, g2, b2 = (80, 70, 255)
    # r2, g2, b2 = (0, 0, 200)
    # r2, g2, b2 = (0, 0, 0)
    r2, g2, b2 = (r, g, b)

    r2 = int(os.environ.get("text2img_r2", r2))
    g2 = int(os.environ.get("text2img_g2", g2))
    b2 = int(os.environ.get("text2img_b2", b2))

    # for i in (1,2,3):
    # for i in []:
    # for i in (1, 2,):
    for i in (1,):
        draw.text((x+i, y), bidi_text, fill=(r2, g2, b2), font=font, embedded_color=True)
        draw.text((x-i, y), bidi_text, fill=(r2, g2, b2), font=font, embedded_color=True)
        draw.text((x, y+i), bidi_text, fill=(r2, g2, b2), font=font, embedded_color=True)
        draw.text((x, y-i), bidi_text, fill=(r2, g2, b2), font=font, embedded_color=True)
        draw.text((x+i, y+i), bidi_text, fill=(r2, g2, b2), font=font, embedded_color=True)
        draw.text((x-i, y-i), bidi_text, fill=(r2, g2, b2), font=font, embedded_color=True)
        draw.text((x-i, y+i), bidi_text, fill=(r2, g2, b2), font=font, embedded_color=True)
        draw.text((x+i, y-i), bidi_text, fill=(r2, g2, b2), font=font, embedded_color=True)

    if False:
    # if True:
        r2, g2, b2 = (r, g, b)
        i = 1
        # draw.text((x+i, y), bidi_text, fill=(r2, g2, b2), font=font, embedded_color=True)
        # draw.text((x-i, y), bidi_text, fill=(r2, g2, b2), font=font, embedded_color=True)
        # draw.text((x, y+i), bidi_text, fill=(r2, g2, b2), font=font, embedded_color=True)
        # draw.text((x, y-i), bidi_text, fill=(r2, g2, b2), font=font, embedded_color=True)
        draw.text((x+i, y+i), bidi_text, fill=(r2, g2, b2), font=font, embedded_color=True)
        draw.text((x-i, y-i), bidi_text, fill=(r2, g2, b2), font=font, embedded_color=True)
        draw.text((x-i, y+i), bidi_text, fill=(r2, g2, b2), font=font, embedded_color=True)
        draw.text((x+i, y-i), bidi_text, fill=(r2, g2, b2), font=font, embedded_color=True)

draw.text((x, y), bidi_text, fill=(r, g, b), font=font, embedded_color=True)

if imgA.any():
    imgB = numpy.array(image)
    imgB = imgB.astype(float)
    # imgOut = blend_modes.soft_light(imgA, imgB, 1.0)
    # imgOut = blend_modes.lighten_only(imgA, imgB, 0.85)
    # imgOut = blend_modes.dodge(imgA, imgB, 0.9)
    # imgOut = blend_modes.addition(imgA, imgB, 0.6)
    # imgOut = blend_modes.darken_only(imgA, imgB, 1.0)
    # imgOut = blend_modes.multiply(imgA, imgB, 1.0)
    # imgOut = blend_modes.hard_light(imgA, imgB, 0.9)
    # imgOut = blend_modes.difference(imgA, imgB, 1.0)
    # imgOut = blend_modes.subtract(imgA, imgB, 1.0)
    # imgOut = blend_modes.grain_extract(imgA, imgB, 1.0)
    # imgOut = blend_modes.divide(imgA, imgB, 1.0)
    ##
    # imgOut = blend_modes.grain_merge(imgA, imgB, 1.0)
    # imgOut = blend_modes.grain_merge(imgOut, imgB, 1.0)
    ##
    # imgOut = blend_modes.difference(imgA, imgB, 1.0)
    # imgOut = blend_modes.overlay(imgOut, imgB, 1.0)
    ##
    # imgOut = blend_modes.overlay(imgA, imgB, 1.0)
    imgOut = blend_modes.difference(imgA, imgB, 0.1)
    imgOut = blend_modes.overlay(imgOut, imgB, 1.0)
    imgOut = blend_modes.overlay(imgOut, imgB, 1.0)
    # imgOut = blend_modes.overlay(imgOut, imgB, 1.0)
    # imgOut = blend_modes.overlay(imgOut, imgB, 1.0)
    # imgOut = blend_modes.overlay(imgOut, imgB, 1.0)
    # imgOut = blend_modes.overlay(imgOut, imgB, 1.0)
    ##
    # imgOut = blend_modes.overlay(imgOut, imgB, 0.5)
    # imgOut = blend_modes.multiply(imgOut, imgB, 1.0)
    # imgOut = blend_modes.dodge(imgOut, imgB, 0.8)
    # imgOut = blend_modes.addition(imgOut, imgB, 1.0)
    # imgOut = blend_modes.soft_light(imgOut, imgB, 1.0)
    # imgOut = blend_modes.hard_light(imgOut, imgB, 1.0)
    ##
    # Save images
    imgOut = numpy.uint8(imgOut)
    imgOut = Image.fromarray(imgOut)
    imgOut.save(imageOut, 'PNG')
else:
    image.save(imageOut, "PNG")
