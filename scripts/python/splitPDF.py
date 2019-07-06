#!/usr/bin/env python
from PyPDF2 import PdfFileWriter, PdfFileReader
import os, sys

if len(sys.argv) < 2 or sys.argv[1]=='-h' or sys.argv[1]=='--help':
    sys.exit("splitPDF.py <file> [<split-to-this-many-parts>]")
input_add = sys.argv[1]
input_add_base = os.path.splitext(input_add)[0] + "_p"
split_num = 3
if len(sys.argv) >= 3:
    split_num = int(sys.argv[2])
input1 = PdfFileReader(open(input_add, "rb"))

p_num = input1.numPages
part_num = int(p_num / split_num) + 1
for i in range(split_num):
    output = PdfFileWriter()
    for j in range(part_num):
        cpn = i * part_num + j
        if (cpn >= p_num):
            break
        output.addPage(input1.getPage(cpn))
    output.write(open(f"{input_add_base}{i}.pdf", "wb"))
