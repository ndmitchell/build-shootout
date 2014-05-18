#!/usr/bin/env python
from fabricate import *

def list():
    run('sh','monad2-run','source','--','list')

def output():
    list()
    run('sh','-c','cat list | xargs cat > output')

main()
