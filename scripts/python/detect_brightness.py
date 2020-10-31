#!/usr/bin/env python3

import os
import sys
import numpy as np
from numpy.linalg import norm
# import matplotlib.image as mpimg # imageio.imread
import cv2 # pip install opencv-python

# data = np.array([mpimg.imread(name) for name in sys.argv[1:]], dtype=np.float64)
data = [cv2.imread(name) for name in sys.argv[1:]]


def brightness1(img):
    if len(img.shape) == 3:
        # Colored RGB or BGR (*Do Not* use HSV images with this function)
        # create brightness with euclidean norm
        return (np.average(norm(img, axis=2)) / np.sqrt(3))/255
    else:
        # Grayscale
        return np.average(img)/255


def brightness2(img):
    img_yuv = cv2.cvtColor(img, cv2.COLOR_BGR2YUV)
    y, u, v = cv2.split(img_yuv)
    return np.average(y)/255

if os.environ.get("detect_brightness_mode", "2") == "1":
    print(brightness1(data[0]))
else:
    print(brightness2(data[0]))
