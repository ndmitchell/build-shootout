
-- | A test script to check those build systems claiming to implement a test
--   do in fact do so.
module Main(main) where

import Util


main :: IO ()
main = do
    test "basic" basic
    test "parallel" parallel


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
    run [Parallel 2, Contents "log" "start\nstart\nend\nend\n"]
    run [NoChange]
