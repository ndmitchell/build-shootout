from fabricate import *

def sources():
    run('sh','multiple-gen','input','--','source1','source2')

def output1():
    sources()
    run('sh','multiple-run','source1','--','output1')

def output2():
    sources()
    run('sh','multiple-run','source2','--','output2')

main()
