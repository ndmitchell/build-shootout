#!/usr/bin/env python
from fabricate import *

def build():
    run('gcc','-c','include-main.c','-o','main.o')

main()
