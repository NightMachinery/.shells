#!/usr/bin/env python3
##
# * [[https://stackoverflow.com/questions/65596323/how-to-programmatically-apply-color-filters-on-the-screen][objective c - How to programmatically apply color filters on the screen? - Stack Overflow]]
#
# * https://github.com/brettferdosi/grayscale/blob/master/Sources/Bridge.h
##
# * [[https://gist.github.com/danielpunkass/df0d72be11b8956f2ef4f4d52cce7a41][Standalone tool using private Apple framework to toggle display grayscale mode]]
# include <stdio.h>

# // Compile with cc -o gray gray.c -framework UniversalAccess -F /System/Library/PrivateFrameworks

# extern void UAGrayscaleSetEnabled(int isEnabled);
# extern int UAGrayscaleIsEnabled();

# int main() {
#         UAGrayscaleSetEnabled(!UAGrayscaleIsEnabled());
# }
##
# lib.MADisplayFilterPrefSetCategoryEnabled(1, True)
# lib.MADisplayFilterPrefGetCategoryEnabled(1)
##

import os
import sys
from ctypes import cdll
lib = cdll.LoadLibrary("/System/Library/PrivateFrameworks/UniversalAccess.framework/UniversalAccess")

color = os.environ.get('screen_color_filter_color', 'gray')
enable_p = os.environ.get('screen_color_filter_enable_p', 'toggle')
if color == 'gray':
    if enable_p == 'toggle':
        enable_p = lib.UAGrayscaleIsEnabled() == 0
    elif enable_p == 'return':
        print(not (lib.UAGrayscaleIsEnabled() == 0))
        sys.exit()
    elif enable_p[0].lower() == 'y':
        enable_p = True
    else:
        enable_p = False

    lib.UAGrayscaleSetEnabled(enable_p)
