
-- | A test script to check those build systems claiming to implement a test
--   do in fact do so.
module Main(main) where

import Util


main :: IO ()
main = do
    test "basic" basic


basic :: ([Opt] -> IO ()) -> IO ()
basic run = do
    writeFile "input" "xyz"
    run [Contents "output" "xyz"]
    run [NoChange]
    writeFile "input" "abc"
    run [Contents "output" "abc"]
    run [NoChange]
