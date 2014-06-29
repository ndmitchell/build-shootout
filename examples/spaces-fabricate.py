#!/usr/bin/env python
from fabricate import *
import sys

def output_file():
    run('cp','input file','output file')


# Hacky, perhaps a bug that can be fixed by fabricate?
for i, x in enumerate(sys.argv):
    if x == 'output file':
        sys.argv[i] = 'output_file'

main()
