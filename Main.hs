
-- | A test script to check those build systems claiming to implement a test
--   do in fact do so.
module Main(main) where

import Util


main :: IO ()
main = do
    test "basic" basic
    test "parallel" parallel
    test "include" include


basic :: ([Opt] -> IO ()) -> IO ()
basic run = do
    writeFile "input" "xyz"
    run [Contents "output" "xyz"]
    run [NoChange]
    writeFile "input" "abc"
    run [Contents "output" "abc"]
    run [NoChange]


parallel :: ([Opt] -> IO ()) -> IO ()
parallel run = do
    writeFile "input1" "xyz"
    writeFile "input2" "abc"
    run [Parallel 2, Contents ".log" "start\nstart\nend\nend\n"]
    run [NoChange]


include :: ([Opt] -> IO ()) -> IO ()
include run = do
    run [Change "main.o"]
    run [NoChange]
    touch "include-2.h"
    run [Change "main.o"]
    run [NoChange]
