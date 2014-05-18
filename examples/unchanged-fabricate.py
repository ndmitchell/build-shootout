#!/usr/bin/env python
from fabricate import *

def source():
    run('sh','unchanged-gen','input','--','source')

def output():
    source()
    run('sh','unchanged-run','source','--','output')

main()
