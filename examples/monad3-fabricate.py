from fabricate import *

def input1():
    pass # source file

def gen():
    run('sh','monad3-gen','--','gen')

def list():
    run('sh','monad3-run','source','--','list')

def output():
    list()
    array = []
    with open('list', 'r') as file:
        for line in file:
            line = ''.join([x for x in line if x != '\n'])
            array.append(line)
            if line == "gen": gen()
    run('sh','-c','cat ' + ' '.join(array) + ' > output')

main()
