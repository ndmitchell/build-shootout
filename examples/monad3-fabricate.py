#!/usr/bin/env python
from fabricate import *

def input1():
    pass # source file

def gen():
    run('sh','monad3-gen','--','gen')

def lst():
    run('sh','monad3-run','source','--','list')

def output():
    lst()
    array = []
    with open('list', 'r') as f:
        for line in f:
            line = line.replace('\n', '')
            array.append(line)
            if line == "gen": gen()
    run('sh','-c','cat ' + ' '.join(array) + ' > output')

main()
