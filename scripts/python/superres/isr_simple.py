#!/usr/bin/env python3
# Usage: isr_simple.py <input_path> <output_path>
# Needs wireguard to download the models
# prereqs:
# `pip install ISR`
##

import sys
import numpy as np
from PIL import Image

input_path = sys.argv[1]
out_path = sys.argv[2]

img = Image.open(input_path)
##
img = img.convert("RGB")
# remove alpha channel
# https://github.com/idealo/image-super-resolution/issues/151
##
lr_img = np.array(img)

###

from ISR.models import RDN

# https://github.com/idealo/image-super-resolution#pre-trained-networks
# https://github.com/idealo/image-super-resolution/blob/master/notebooks/ISR_Prediction_Tutorial.ipynb
model = RDN(weights='psnr-small')

sr_img = model.predict(lr_img, by_patch_of_size=100)
img_out = Image.fromarray(sr_img)
img_out.save(out_path, 'PNG')
