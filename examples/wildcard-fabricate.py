#!/usr/bin/env python
from fabricate import *
import os, glob

def build():
    for f in glob.glob('*.in'):
        run('cp', f, os.path.splitext(f)[0]+'.out')

main()
