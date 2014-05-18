#!/usr/bin/env python
from fabricate import *

def output():
    run('sh','-c','cat list | xargs cat > output')

main()
