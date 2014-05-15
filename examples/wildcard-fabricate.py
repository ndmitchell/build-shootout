#!/usr/bin/env python
from fabricate import *
import sys, os, glob

def get_cp(inf, out):
    @staticmethod
    def _f(): run('cp', inf, out)
    return _f

for f in glob.glob('*.in'):
    base = os.path.splitext(f)[0]
    globals()[base] = type(base, (object,), {'out': get_cp(f, base+'.out')})

#sys.argv[1:] = [(arg[:-4]+'__DOT_OUT') if arg.endswith('.out') else arg\
#    for arg in sys.argv[1:]]

main()
